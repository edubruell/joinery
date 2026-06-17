# Disable embedding reuse for the test baseline so the embedding tests embed
# fresh on every call (their fixtures share text under a NULL model, which would
# otherwise collide in the global cache). Tests that exercise reuse opt back in
# locally with `withr::local_options(joinery.embedding_reuse = TRUE)`.
withr::local_options(
  joinery.embedding_reuse = FALSE,
  .local_envir = testthat::teardown_env()
)
