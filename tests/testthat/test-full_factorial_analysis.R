test_that("Calculating full factorial main and interactive effects works", {
  x1=c(1,2,3,4,5,6)
  x2=c(1,2,3)
  #model: only depends on x2
  df1=expand.grid(x1,x2)
  colnames(df1)=c("x1","x2")
  df1$y=3*df1$x2
  effects=calculate_effects_ff("y",c("x1","x2"),df1)
  expect_equal(effects$main_effect[effects$factor=="x1"],0)
  expect_equal(effects$main_effect[effects$factor=="x2"],1)
  expect_equal(sum(effects$interaction_part),0)

#model: depends half on x1 and 5% half on x2
  x1=c(1,2,3)
  x2=c(1,2,3)
  df2=expand.grid(x1,x2)
  colnames(df2)=c("x1","x2")
  df2$y=0.5*df2$x1-0.5*df2$x2
  effects=calculate_effects_ff("y",c("x1","x2"),df2)
  expect_equal(effects$main_effect[effects$factor=="x1"],effects$main_effect[effects$factor=="x2"])
  expect_equal(sum(effects$interaction_part),0)

  #model: depends on x1, x2, and their interaction
  x1=1:5
  x2=0:1
  df3=expand.grid(x1,x2)
  colnames(df3)=c("x1","x2")
  df3$y[df3$x2==1]=x1
  df3$y[df3$x2==0]=3*x1
  effects=calculate_effects_ff("y",c("x1","x2"),df3)
  expect_gt(sum(effects$interaction_part),0)
  expect_equal(effects$interaction_part[effects$factor=="x1"],effects$interaction_part[effects$factor=="x2"])
})
