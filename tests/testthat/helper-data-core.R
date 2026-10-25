core_bands <- function() data.frame(wavelength = c(500, 600, 700, 800))
core_geometry <- function() {
  list(
    origin = c(0, 0),
    transform = diag(3)
  )
}
core_image <- function() {
  hm_dataset(
    array(
      seq_len(60),
      c(3, 5, 4)
    ),
    data.frame(observation_id = "o", image_id = "i", acquisition_id = "a"),
    core_bands(), core_geometry()
  )
}
core_sequence <- function() {
  frames <- list(
    array(1:60, c(3, 5, 4)),
    array(61:120, c(3, 5, 4))
  )
  fi <- data.frame(
    frame_id = c("f1", "f2"),
    sequence_id = "s", acquisition_id = "a", time = c(0, 2.5)
  )
  obs <- data.frame(
    observation_id = c("o1", "o2"),
    image_id = c("i1", "i2"),
    acquisition_id = "a", frame_id = c("f1", "f2")
  )
  hm_sequence(frames, fi, obs, core_bands(), core_geometry())
}
