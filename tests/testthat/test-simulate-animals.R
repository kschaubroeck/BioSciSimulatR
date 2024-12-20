
test_that("simulate_dams returns correct number of rows", {
  result <- simulate_dams(
    dams_per_group = 10,
    group1 = c("A", "B"),
    group2 = c("C", "D", "E"),
    simulations = 3
  )

  expect_s3_class(result, "data.frame")

  # 6 groups (2 group1 * 3 group2)
  # 10 dams per group * 6 groups * 3 simulations = 180
  expect_equal(nrow(result), 180)

  # Each simulation should produce 60 items, 10 in each group
  simulation_group_counts <- table(result$group1, result$group2, result$simulation)
  expect_true(all(simulation_group_counts == 10))

  # Litters should also be unique per simulation
  simulation_litter_counts <- table(result$litter, result$simulation)
  expect_true(all(simulation_litter_counts == 1))
})

test_that("simulate_dams returns correct number of batches", {
  result <- simulate_dams(
    dams_per_group = 5,
    group1 = c("A", "B"),
    simulations = 2,
    batches = 3
  )

  # There are 5 dams per group which means 1.66 dams per batch per group
  # 2 batches will get 2 dams, 1 batch will get 1 dams
  # 2 batches will get 1 dam, 1 batch will get 3 dams
  simulation_batch_counts <- table(result$batch, result$group1, result$simulation)

  # Check that each column in the table meets the criteria
  batch_counts_A_sim1 <- simulation_batch_counts[, "A", 1]
  batch_counts_A_sim2 <- simulation_batch_counts[, "A", 2]
  batch_counts_B_sim1 <- simulation_batch_counts[, "B", 1]
  batch_counts_B_sim2 <- simulation_batch_counts[, "B", 2]

  # Calc expectations
  expect_true(
    all(sort(batch_counts_A_sim1) == c(1, 2, 2)) ||
      all(sort(batch_counts_A_sim1) == c(1, 1, 3))
  )
  expect_true(
    all(sort(batch_counts_A_sim2) == c(1, 2, 2)) ||
      all(sort(batch_counts_A_sim2) == c(1, 1, 3))
  )
  expect_true(
    all(sort(batch_counts_B_sim1) == c(1, 2, 2)) ||
      all(sort(batch_counts_B_sim1) == c(1, 1, 3))
  )
  expect_true(
    all(sort(batch_counts_B_sim2) == c(1, 2, 2)) ||
      all(sort(batch_counts_B_sim2) == c(1, 1, 3))
  )
})

test_that("simulate_real_litters respects min_litter_size and max_litter_size", {
  # quick throw together a dams test data frame
  dams <- data.frame(
    simulation = factor(rep(1:2, each = 6)),
    treatment = factor(rep(c("C", "T"), times = 6)),
    litter = factor(rep(1:6, times = 2))
  )

  # Does not set a seed so the results are random each time which I think is okay
  # since this test SHOULD pass for any value of the seed.
  result <- simulate_real_litters(dams, mean_litter_size = 6, min_litter_size = 3, max_litter_size = 8)

  # Right class ?
  expect_s3_class(result, "data.frame")

  # Right sizes
  litter_sizes <- table(result$litter, result$simulation)
  expect_true(all(litter_sizes >= 3))
  expect_true(all(litter_sizes <= 8))
})

test_that("simulate_real_litters handles additional biological groups correctly", {
  dams <- data.frame(
    simulation = factor(rep(1:2, each = 6)),
    treatment = factor(rep(c("C", "T"), times = 6)),
    litter = factor(rep(1:6, times = 2))
  )
  result <- simulate_real_litters(
    dams,
    mean_litter_size = 6,
    min_litter_size = 1,
    max_litter_size = 16,
    sex = c(female = 0.5, male = 0.5)
  )

  # By correctly we mean show up with the right types
  expect_true("sex" %in% colnames(result))
  expect_true(is.factor(result[["sex"]]))
  expect_true(all(result$sex %in% c("male", "female")))
})


test_that("simulate_ideal_litters works correctly", {
  dams <- data.frame(
    simulation = factor(rep(1:2, each = 6)),
    treatment = factor(rep(c("C", "T"), times = 6)),
    litter = factor(rep(1:6, times = 2))
  )
  result <- simulate_ideal_litters(
    dams,
    group1 = c(A = 2, B = 4),
    group2 = c(X = 2, Y = 3),
    offspring_per_litter_group = 2
  )

  # Right class ?
  expect_s3_class(result, "data.frame")

  # Check if result has the correct columns (and retains info from the dams)
  expect_true(all(c("litter", "group1", "group2", "treatment") %in% colnames(result)))

  # Ensure correct counts for different groups
  # The 4 groups (AX, AY, BX, BY) should have (4, 6, 8, 12) * 2 (offspring_per_litter_group)
  simulation_batch_counts <- table(result$group1, result$group2, result$simulation, result$litter)
  expect_true(all(simulation_batch_counts["A","X",,] == 8))
  expect_true(all(simulation_batch_counts["A","Y",,] == 12))
  expect_true(all(simulation_batch_counts["B","X",,] == 16))
  expect_true(all(simulation_batch_counts["B","Y",,] == 24))
})


test_that("simulate_repeated_measurements works with numeric replicates", {
  animals <- data.frame(
    simulation = factor(c(1,1,2,2)),
    value = c(1, 2, 3, 4)
  )
  result <- simulate_repeated_measurements(animals, 2, "simulation")

  # Expect number of items and the appropriate levels
  expect_equal(nrow(result), 8)
  expect_equal(levels(result$replicate), c("R1", "R2"))

  # Expect the values to be replicated;
  simulation_replicates <- table(result$animal,result$simulation)
  expect_true(all(simulation_replicates == 2))
})
