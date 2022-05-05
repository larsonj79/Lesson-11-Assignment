library(testthat)

# each call to test_that() produces one test
# each test represents one point value
# you can have multiple tests for each question

library(readxl)
library(ggplot2)
library(dplyr)
fskey <- read_excel("Father-son-height.xlsx")

plotfskey <- ggplot(fskey, aes(x = Father, y = Son)) +
  geom_point()

plotfs2key <- ggplot(fskey, aes(x = Father, y = Son)) +
  geom_point() +
  geom_abline(slope = 1.025627, intercept = -.737, color = "red")

plotfs3key <- ggplot(fskey, aes(x = Father, y = Son)) +
  geom_point() +
  geom_abline(slope = 1.025627, intercept = -.737, color = "red") +
  geom_abline(slope = htslope, intercept = htint)

test_that("Q1 plot (visible)", {
  
  expect_equal(plotfs$layers[[1]], plotfskey$layers[[1]])
  expect_equal(plotfs$scales, plotfskey$scales)
  expect_equal(plotfs$mapping, plotfskey$mapping)
  expect_equal(plotfs$labels, plotfskey$labels)
  
})

test_that("Q2 (visible)", {
  
  expect_equal(summstats$meanf, 67.68683, tolerance = 1e-2)
  expect_equal(summstats$means, 68.68423, tolerance = 1e-2)
  expect_equal(summstats$sdf, 2.745827, tolerance = 1e-2)
  expect_equal(summstats$maxs, 78.4, tolerance = 1e-2)
  expect_equal(summstats$minf, 59, tolerance = 1e-2)
  
})

test_that("Q5 plot (visible)", {
  
  expect_equal(plotfs2$layers[[1]], plotfs2key$layers[[1]])
  expect_equal(plotfs2$scales, plotfs2key$scales)
  expect_equal(plotfs2$mapping, plotfs2key$mapping)
  expect_equal(plotfs2$labels, plotfs2key$labels)
  
})

test_that("Q9 (visible)", {
  
  expect_equal(htslope, .5140059, tolerance = 1e-2)
  
})

test_that("Q10 (visible)", {
  
  expect_equal(htint, 33.8928, tolerance = 1e-2)
  
})

test_that("Q11 plot (visible)", {
  
  expect_equal(plotfs3$layers[[1]], plotfs3key$layers[[1]])
  expect_equal(plotfs3$scales, plotfs3key$scales)
  expect_equal(plotfs3$mapping, plotfs3key$mapping)
  expect_equal(plotfs3$labels, plotfs3key$labels)
  
})

test_that("Q14 (visible)", {
  
  expect_equal(f72, 73, tolerance = 1e-2)
  
})

test_that("Q15 (visible)", {
  
  expect_equal(s72, 127, tolerance = 1e-2)
  
})

test_that("Q15 (visible)", {
  
  expect_equal(sonest72, 70.90123, tolerance = 1e-2)
  
})

test_that("Q18 (visible)", {
  
  expect_equal(as.numeric(htreg$coefficients[1]), 33.8928005, tolerance = 1e-2)
  expect_equal(as.numeric(htreg$coefficients[2]), .5140059, tolerance = 1e-2)
  expect_equal(as.numeric(htreg$fitted.values[2]), 66.42937, tolerance = 1e-2)
  expect_equal(as.numeric(htreg$fitted.values[7]), 67.50879, tolerance = 1e-2)
  
})

test_that("Q22 (visible)", {
  
  expect_true(names(nbaht) == "Father")
  expect_true(class(nbaht) == "data.frame")

})

test_that("Q23 (visible)", {
  
  expect_equal(as.numeric(predson[1]),  80.1533, tolerance = 1e-3)
  expect_equal(as.numeric(predson[2]),  77.5833, tolerance = 1e-3)
  expect_equal(as.numeric(predson[5]),  79.12532, tolerance = 1e-3)

})
