use anyhow::Context;
use serde::{de::Error, Deserialize, Deserializer, Serialize, Serializer};
use std::{
    fmt::{self, Display},
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
    time::SystemTime,
};
use tracing::debug;
use relative_path::RelativePath;

pub(crate) mod iterator;
use iterator::language;

pub use iterator::{BranchFilter, BranchFilterConfig, FileFilter, FileFilterConfig, FilterUpdate};

// Types of repo
#[derive(Serialize, Deserialize, Hash, PartialEq, Eq, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub enum Backend {
    Local,
    Github,
}

// Repository identifier
#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub struct RepoRef {
    pub name: String,
}

pub fn from_path<P: ?Sized + AsRef<path::Path>>(path: &P) -> Result<&RelativePath, FromPathError> {
    use std::path::Component::*;

    let other = path.as_ref();

    let s = match other.to_str() {
        Some(s) => s,
        None => return Err(FromPathErrorKind::NonUtf8.into()),
    };

    let rel = RelativePath::new(s);

    // check that the component compositions are equal.
    for (a, b) in other.components().zip(rel.components()) {
        match (a, b) {
            (Prefix(_), _) | (RootDir, _) => return Err(FromPathErrorKind::NonRelative.into()),
            (CurDir, Component::CurDir) => continue,
            (ParentDir, Component::ParentDir) => continue,
            (Normal(a), Component::Normal(b)) if a == b => continue,
            _ => return Err(FromPathErrorKind::BadSeparator.into()),
        }
    }

    Ok(rel)
}

pub fn get_relative_path<P>(path: &Path, base: P) -> PathBuf
where
    P: AsRef<Path>,
{
    RelativePath::from_path(path)
        .map(|rp| rp.to_logical_path(base))
        .unwrap_or_else(|_| path.to_owned())
}

impl RepoRef {
    pub fn new(name: &(impl AsRef<str> + ?Sized)) -> Result<Self, RepoError> {
        let path = Path::new(name.as_ref());

        if !path.is_absolute() {
            return Err(RepoError::NonAbsoluteLocal);
        }

        for component in path.components() {
            use std::path::Component::*;
            match component {
                CurDir | ParentDir => return Err(RepoError::InvalidPath),
                _ => continue,
            }
        }

        Ok(RepoRef {
            name: name.as_ref().to_owned(),
        })
    }

    pub fn from_components(root: &Path, components: Vec<String>) -> Result<Self, RepoError> {
        let refstr = components.join("/");
        let pathstr = match refstr.trim_start_matches('/').split_once('/') {
            Some(("local", name)) => name,
            _ => &refstr,
        };

        let local_path = get_relative_path(Path::new(pathstr), root);
        Self::new(&local_path.to_string_lossy())
    }

    pub fn backend(&self) -> Backend {
        self.backend.clone()
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn is_local(&self) -> bool {
        self.backend == Backend::Local
    }

    pub fn is_remote(&self) -> bool {
        self.backend != Backend::Local
    }

    pub fn indexed_name(&self) -> String {
        Path::new(&self.name)
            .file_name()
            .expect("last component is `..`")
            .to_string_lossy()
            .into()
    }

    pub fn display_name(&self) -> String {
        self.indexed_name()
    }

    pub fn local_path(&self) -> Option<PathBuf> {
        Some(PathBuf::from(&self.name))
    }
}

impl AsRef<RepoRef> for RepoRef {
    fn as_ref(&self) -> &RepoRef {
        self
    }
}

impl<P: AsRef<Path>> From<&P> for RepoRef {
    fn from(path: &P) -> Self {
        assert!(path.as_ref().is_absolute());
        RepoRef {
            name: path.as_ref().to_string_lossy().to_string(),
        }
    }
}

impl From<&str> for RepoRef {
    fn from(refstr: &str) -> Self {
        Self::from_str(refstr).unwrap()
    }
}

impl FromStr for RepoRef {
    type Err = RepoError;

    fn from_str(refstr: &str) -> Result<Self, Self::Err> {
        match refstr.trim_start_matches('/').split_once('/') {
            // local/...
            Some(("local", name)) => RepoRef::new(name),
            _ => Err(RepoError::InvalidBackend),
        }
    }
}

impl Display for RepoRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "local/{}", self.name())
    }
}

impl Serialize for RepoRef {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for RepoRef {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        String::deserialize(deserializer).and_then(|s| {
            RepoRef::from_str(s.as_str()).map_err(|e| D::Error::custom(e.to_string()))
        })
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Repository {
    /// Path to the physical location of the repo root
    pub disk_path: PathBuf,

    /// Current user-readable status of syncing
    pub sync_status: SyncStatus,

    /// Time of last commit at the last successful index
    pub last_commit_unix_secs: i64,

    /// Time of last successful index
    pub last_index_unix_secs: u64,

    /// Most common language
    pub most_common_lang: Option<String>,

    /// Filters which branches to index
    pub branch_filter: Option<BranchFilterConfig>,

    /// Custom file filter overrides
    #[serde(default)]
    pub file_filter: FileFilterConfig,
}

impl Repository {
    /// Only use this with local refs
    ///
    /// # Panics
    ///
    /// When used with non-local refs
    pub(crate) fn local_from(reporef: &RepoRef) -> Self {
        let disk_path = reporef.local_path().unwrap();

        Self {
            sync_status: SyncStatus::Queued,
            last_index_unix_secs: 0,
            last_commit_unix_secs: 0,
            most_common_lang: None,
            branch_filter: None,
            file_filter: Default::default(),
            disk_path,
        }
    }

    /// Pre-scan the repository to provide supporting metadata for a
    /// new indexing operation
    pub async fn get_repo_metadata(&self) -> Arc<RepoMetadata> {
        let last_commit_unix_secs = gix::open(&self.disk_path)
            .context("failed to open git repo")
            .and_then(|repo| Ok(repo.head()?.peel_to_commit_in_place()?.time()?.seconds))
            .ok();

        let langs = Default::default();

        RepoMetadata {
            last_commit_unix_secs,
            langs,
        }
        .into()
    }

    /// Marks the repository for removal on the next sync
    /// Does not initiate a new sync.
    pub(crate) fn mark_removed(&mut self) {
        self.sync_status = SyncStatus::Removed;
    }

    /// Marks the repository for indexing on the next sync
    /// Does not initiate a new sync.
    pub(crate) fn mark_queued(&mut self) {
        self.sync_status = SyncStatus::Queued;
    }

    pub(crate) fn sync_done_with(
        &mut self,
        filter_update: &FilterUpdate,
        metadata: Arc<RepoMetadata>,
    ) {
        self.last_index_unix_secs = get_unix_time(SystemTime::now());
        self.last_commit_unix_secs = metadata.last_commit_unix_secs.unwrap_or(0);
        self.most_common_lang = metadata
            .langs
            .most_common_lang()
            .map(|l| l.to_string())
            .or_else(|| self.most_common_lang.take());

        if let Some(ref bf) = filter_update.branch_filter {
            self.branch_filter = bf.patch_into(self.branch_filter.as_ref());
        }

        if let Some(ref ff) = filter_update.file_filter {
            self.file_filter = ff.patch_into(&self.file_filter);
        }

        self.sync_status = SyncStatus::Done;
    }
}

fn get_unix_time(time: SystemTime) -> u64 {
    time.duration_since(SystemTime::UNIX_EPOCH)
        .expect("system time error")
        .as_secs()
}

#[derive(Debug)]
pub struct RepoMetadata {
    pub last_commit_unix_secs: Option<i64>,
    pub langs: language::LanguageInfo,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug, Hash)]
#[serde(rename_all = "snake_case")]
pub enum SyncStatus {
    /// There was an error during last sync & index
    Error { message: String },

    /// Repository is not yet managed by bloop
    Uninitialized,

    /// Removed by the user
    Removed,

    /// The user requested cancelling the process
    Cancelling,

    /// Last sync & index cancelled by the user
    Cancelled,

    /// Queued for sync & index
    Queued,

    /// Active VCS operation in progress
    Syncing,

    /// Active indexing in progress
    Indexing,

    /// VCS remote has been removed
    RemoteRemoved,

    /// Successfully indexed
    Done,
}

impl SyncStatus {
    pub(crate) fn indexable(&self) -> bool {
        use SyncStatus::*;
        matches!(self, Queued | Done | Error { .. })
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub struct GitRemote {
    /// protocol to use during git operations
    pub protocol: GitProtocol,
    /// Hostname of provider
    pub host: String,
    /// any kind of `protocol` and [`Backend`]-dependent address
    pub address: String,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub enum GitProtocol {
    Https,
    Ssh,
}

#[derive(thiserror::Error, Debug)]
pub enum RepoError {
    #[error("no source configured")]
    NoSourceGiven,
    #[error("local repository must have an absolute path")]
    NonAbsoluteLocal,
    #[error("paths can't contain `..` or `.`")]
    InvalidPath,
    #[error("backend not recognized")]
    InvalidBackend,
    #[error("IO error: {error}")]
    IO {
        #[from]
        error: std::io::Error,
    },
    #[error("invalid state file")]
    Decode {
        #[from]
        error: serde_json::Error,
    },
    #[error("indexing error")]
    Anyhow {
        #[from]
        error: anyhow::Error,
    },
}
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_reporef() {
        assert_eq!(
            "/tmp/repository".parse::<RepoRef>().unwrap(),
            RepoRef::new("/tmp/repository").unwrap()
        );
        if "repository".parse::<RepoRef>().is_ok() {
            panic!("non-absolute local allowed")
        }
    }

    #[test]
    fn serialize_reporef() {
        assert_eq!(
            r#""local//org/repo""#,
            &serde_json::to_string(&RepoRef::new("/org/repo").unwrap()).unwrap()
        );
    }
}