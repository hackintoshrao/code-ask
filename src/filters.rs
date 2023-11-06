use regex::RegexSet;
pub enum BranchFilter {
    All,
    Head,
    Select(RegexSet),
}

impl BranchFilter {
    pub fn filter(&self, is_head: bool, branch: &str) -> bool {
        match self {
            BranchFilter::All => true,
            BranchFilter::Select(patterns) => is_head || patterns.is_match(branch),
            BranchFilter::Head => is_head,
        }
    }
}

impl Default for BranchFilter {
    fn default() -> Self {
        Self::Head
    }
}