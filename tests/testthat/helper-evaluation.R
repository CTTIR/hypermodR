eval_fixture <- function() {
  t <- hm_task("y", "classification", "spectrum", "subject_id")
  r <- data.frame(
    observation_id = paste0("o", 1:6),
    y = c("a", "a", "b", "b", "a", "b"),
    subject_id = c("s1", "s1", "s1", "s2", "s2", "s2"),
    acquisition_id = paste0("a", 1:6)
  )
  q <- cbind(
    a = c(0.8, 0.3, 0.7, 0.1, 0.6, 0.4),
    b = c(0.2, 0.7, 0.3, 0.9, 0.4, 0.6)
  )
  p <- hm_prediction(
    c("a", "b", "a", "b", "a", "b"),
    t, r$observation_id, q, c("a", "b")
  )
  list(p = p, r = r)
}
