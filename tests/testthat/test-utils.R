test_that("Loading LUT for time series works", {
  lut1=lut_tscode("3")
  lut2=lut_tscode()
  lut3=lut_tscode(3)
  expect_all_equal(c(lut1$TimeSeriesType,lut2$TimeSeriesType[lut2$Type==3 & !is.na(lut2$Type)],lut3$TimeSeriesType),"Fishing effort")
})
