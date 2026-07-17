test_that("Loading LUT for time series works", {
  lut1=lut_tscode("3")
  lut2=lut_tscode()
  lut3=lut_tscode(3)
  expect_all_equal(c(lut1$TimeSeriesType,lut2$TimeSeriesType[lut2$Type==3 & !is.na(lut2$Type)],lut3$TimeSeriesType),"Fishing effort")
})

test_that("Gradually increasing (or decreasing) a vector over time by a summand works", {
 v1=1:10
 v2=change_values_add(v1,1,3,6)
 expect_all_true(v1[1:2]==v2[1:2])
 expect_all_true(v1[6:10]==v2[6:10]-1)
 expect_true(v2[4]==v1[4]+0.5)
})

test_that("Gradually increasing (or decreasing) a vector over time by a multiplier works", {
  v1=1:10
  v2=change_values_mult(v1,1,3,6)
  expect_all_true(v1==v2)
  v3=change_values_mult(v1,1.5,3,6)
  expect_all_true(v1[1:2]==v3[1:2])
  expect_all_true(v1[6:10]==v3[6:10]/1.5)
  expect_true(v3[4]==v1[4]+0.5/2*v1[4])
})
