library(testthat)

# each call to test_that() produces one test
# each test represents one point value
# you can have multiple tests for each question

library(readxl)
library(ggplot2)
library(dplyr)
fexp <- read_excel("FieldExperiment.xlsx")
fexp <- data.frame(fexp)
fexp$DATE <- as.Date(fexp$DATE)
fexp$WEEK <- factor(fexp$WEEK)

fexpnykey <- fexp %>% 
  filter(DMA_NAME == "New York, NY")

FBProspkey <- ggplot(fexpnykey, aes(x = FACEBOOK_PROSPECTING_CLICKS, y = FACEBOOK_PROSPECTING_SPEND)) +
  geom_point()

FBProsp2key <- ggplot(fexpnykey, aes(x = FACEBOOK_PROSPECTING_CLICKS, 
                               y = FACEBOOK_PROSPECTING_SPEND,
                               size = SHOPIFY_US_SALES)) +
  geom_point()

fexp_nylakey <- fexp %>% 
  filter(DMA_NAME %in% c("New York, NY", "Los Angeles, CA"))

FBRetkey <- ggplot(data = fexp_nylakey, aes(x = FACEBOOK_RETARGETING_CLICKS,
                                      y = FACEBOOK_RETARGETING_SPEND,
                                      size = SHOPIFY_US_SALES,
                                      color = DMA_NAME)) +
  geom_point(alpha = .5)

FBRet2key <- ggplot(data = fexp_nylakey, aes(x = FACEBOOK_RETARGETING_CLICKS,
                                       y = FACEBOOK_RETARGETING_SPEND,
                                       size = SHOPIFY_US_SALES,
                                       shape = DMA_NAME)) +
  geom_point(alpha = .5)

fexp_top51key <- fexp %>% 
  filter(POPN > 600000)

GPC51key <- ggplot(data = fexp_top51key, aes(x = GOOGLE_PROSPECTING_IMPRESSIONS, 
                                       y = GOOGLE_PROSPECTING_CLICKS)) +
  geom_point(alpha = .5)

GPC51bkey <- fexp_top51key %>% 
  group_by(DMA_NAME) %>% 
  summarize(TotImp = sum(GOOGLE_PROSPECTING_IMPRESSIONS),
            TotClick = sum(GOOGLE_PROSPECTING_CLICKS)) %>% 
  ggplot(aes(x = TotImp, y = TotClick)) +
  geom_point(alpha = .5)

top51_ratiokey <- fexp_top51key %>% 
  group_by(DMA_NAME) %>% 
  summarize(TotSales = sum(SHOPIFY_US_SALES),
            POPN = mean(POPN)) %>% 
  mutate(SalesPerCap = TotSales / POPN)

SPChistkey <- ggplot(data = top51_ratiokey, aes(x = SalesPerCap)) +
  geom_histogram()

SPCBarkey <- ggplot(data = top51_ratiokey, aes(x = reorder(DMA_NAME, -SalesPerCap), y = SalesPerCap)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

GRscatterkey <- ggplot(data = fexp_nylakey, aes(x = GOOGLE_RETARGETING_IMPRESSIONS,
                                          y = GOOGLE_RETARGETING_CLICKS,
                                          color = DMA_NAME,
                                          size = SHOPIFY_US_SALES)) +
  geom_point()

GRscatterbkey <- ggplot(data = fexp_nylakey, aes(x = GOOGLE_RETARGETING_IMPRESSIONS,
                                           y = GOOGLE_RETARGETING_CLICKS,
                                           color = DMA_NAME,
                                           size = SHOPIFY_US_SALES)) +
  geom_point() +
  scale_color_brewer()

GRscatterckey <- ggplot(data = fexp_nylakey, aes(x = GOOGLE_RETARGETING_IMPRESSIONS,
                                           y = GOOGLE_RETARGETING_CLICKS,
                                           color = DMA_NAME,
                                           size = SHOPIFY_US_SALES)) +
  geom_point() +
  scale_color_brewer() +
  theme_classic()

GRscatterdkey <- ggplot(data = fexp_nylakey, aes(x = GOOGLE_RETARGETING_IMPRESSIONS,
                                           y = GOOGLE_RETARGETING_CLICKS,
                                           color = DMA_NAME,
                                           size = SHOPIFY_US_SALES)) +
  geom_point() +
  scale_color_brewer(palette = "Set2") +
  theme_classic()

top51_ratiokey <- top51_ratiokey %>% 
  mutate(POPN2 = ifelse(POPN > 2000000, 3, ifelse(POPN < 1000000, 1, 2)))

SPCBar2key <- ggplot(data = top51_ratiokey, aes(x = reorder(DMA_NAME, -SalesPerCap), 
                                          y = SalesPerCap,
                                          fill = as.factor(POPN2))) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

SPCBar3key <- ggplot(data = top51_ratiokey, aes(x = reorder(DMA_NAME, -SalesPerCap), 
                                          y = SalesPerCap,
                                          fill = as.factor(POPN2))) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90))

popnbarkey <- ggplot(data = top51_ratiokey, aes(x = as.factor(POPN2), 
                                          y = POPN)) +
  geom_bar(stat = "identity")

popnbar2key <- ggplot(data = top51_ratiokey, aes(x = as.factor(POPN2), 
                                           y = POPN,
                                           fill = DMA_NAME)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none")

popnbar3key <- ggplot(data = top51_ratiokey, aes(x = as.factor(POPN2), 
                                           y = POPN,
                                           fill = DMA_NAME)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(legend.position = "none")

popnbar4key <- ggplot(data = top51_ratiokey, aes(x = as.factor(POPN2), 
                                           y = POPN,
                                           fill = DMA_NAME)) +
  geom_bar(stat = "identity", position = "fill") +
  theme(legend.position = "none")

popnbar5key <- ggplot(data = top51_ratiokey, aes(x = as.factor(POPN2), 
                                           y = POPN,
                                           fill = DMA_NAME)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = .5),
           alpha = .5) +
  theme(legend.position = "none")



test_that("Q1 Names (visible)", {
  
  expect_true(dim(fexpny)[1] == 21)
  expect_true(dim(fexpny)[2] == 26)
  expect_true(names(fexpny)[1] == "DATE")
  expect_true(names(fexpny)[4] == "DMA_CONDITION")
  expect_true(names(fexpny)[9] == "GOOGLE_RETARGETING_IMPRESSIONS")

})

test_that("Q2 plot (visible)", {
  
  expect_equal(FBProsp$layers[[1]], FBProspkey$layers[[1]])
  expect_equal(FBProsp$scales, FBProspkey$scales)
  expect_equal(FBProsp$mapping, FBProspkey$mapping)
  expect_equal(FBProsp$labels, FBProspkey$labels)
  
})

test_that("Q3 plot (visible)", {
  
  expect_equal(FBProsp2$layers[[1]], FBProsp2key$layers[[1]])
  expect_equal(FBProsp2$scales, FBProsp2key$scales)
  expect_equal(FBProsp2$mapping, FBProsp2key$mapping)
  expect_equal(FBProsp2$labels, FBProsp2key$labels)
  
})


test_that("Q4 plot (visible)", {
  
  expect_equal(FBRet$layers[[1]], FBRetkey$layers[[1]])
  expect_equal(FBRet$scales, FBRetkey$scales)
  expect_equal(FBRet$mapping, FBRetkey$mapping)
  expect_equal(FBRet$labels, FBRetkey$labels)
  
})


test_that("Q5 plot (visible)", {
  
  expect_equal(FBRet2$layers[[1]], FBRet2key$layers[[1]])
  expect_equal(FBRet2$scales, FBRet2key$scales)
  expect_equal(FBRet2$mapping, FBRet2key$mapping)
  expect_equal(FBRet2$labels, FBRet2key$labels)
  
})


test_that("Q7 plot (visible)", {
  
  expect_equal(GPC51$layers[[1]], GPC51key$layers[[1]])
  expect_equal(GPC51$scales, GPC51key$scales)
  expect_equal(GPC51$mapping, GPC51key$mapping)
  expect_equal(GPC51$labels, GPC51key$labels)
  
})


test_that("Q8 plot (visible)", {
  
  expect_equal(GPC51b$layers[[1]], GPC51bkey$layers[[1]])
  expect_equal(GPC51b$scales, GPC51bkey$scales)
  expect_equal(GPC51b$mapping, GPC51bkey$mapping)
  expect_equal(GPC51b$labels, GPC51bkey$labels)
  
})


test_that("Q10 plot (visible)", {
  
  expect_equal(SPChist$layers[[1]], SPChistkey$layers[[1]])
  expect_equal(SPChist$scales, SPChistkey$scales)
  expect_equal(SPChist$mapping, SPChistkey$mapping)
  expect_equal(SPChist$labels, SPChistkey$labels)
  
})


test_that("Q11 plot (visible)", {
  
  expect_equal(SPCBar$layers[[1]], SPCBarkey$layers[[1]])
  expect_equal(SPCBar$scales, SPCBarkey$scales)
  expect_equal(SPCBar$mapping, SPCBarkey$mapping)
  expect_equal(SPCBar$labels, SPCBarkey$labels)
  
})


test_that("Q14 plot (visible)", {
  
  expect_equal(GRscatter$layers[[1]], GRscatterkey$layers[[1]])
  expect_equal(GRscatter$scales, GRscatterkey$scales)
  expect_equal(GRscatter$mapping, GRscatterkey$mapping)
  expect_equal(GRscatter$labels, GRscatterkey$labels)
  
})


test_that("Q15 plot (visible)", {
  
  expect_equal(GRscatterb$layers[[1]], GRscatterbkey$layers[[1]])
  expect_equal(GRscatterb$scales, GRscatterbkey$scales)
  expect_equal(GRscatterb$mapping, GRscatterbkey$mapping)
  expect_equal(GRscatterb$labels, GRscatterbkey$labels)
  
})

test_that("Q16 plot (visible)", {
  
  expect_equal(GRscatterc$layers[[1]], GRscatterckey$layers[[1]])
  expect_equal(GRscatterc$scales, GRscatterckey$scales)
  expect_equal(GRscatterc$mapping, GRscatterckey$mapping)
  expect_equal(GRscatterc$labels, GRscatterckey$labels)
  
})

test_that("Q17 plot (visible)", {
  
  expect_equal(GRscatterd$layers[[1]], GRscatterdkey$layers[[1]])
  expect_equal(GRscatterd$scales, GRscatterdkey$scales)
  expect_equal(GRscatterd$mapping, GRscatterdkey$mapping)
  expect_equal(GRscatterd$labels, GRscatterdkey$labels)
  
})

test_that("Q19 plot (visible)", {
  
  expect_equal(SPCBar2$layers[[1]], SPCBar2key$layers[[1]])
  expect_equal(SPCBar2$scales, SPCBar2key$scales)
  expect_equal(SPCBar2$mapping, SPCBar2key$mapping)
  expect_equal(SPCBar2$labels, SPCBar2key$labels)
  
})

test_that("Q20 plot (visible)", {
  
  expect_equal(SPCBar3$layers[[1]], SPCBar3key$layers[[1]])
  expect_equal(SPCBar3$scales, SPCBar3key$scales)
  expect_equal(SPCBar3$mapping, SPCBar3key$mapping)
  expect_equal(SPCBar3$labels, SPCBar3key$labels)
  
})

test_that("Q21 plot (visible)", {
  
  expect_equal(popnbar$layers[[1]], popnbarkey$layers[[1]])
  expect_equal(popnbar$scales, popnbarkey$scales)
  expect_equal(popnbar$mapping, popnbarkey$mapping)
  expect_equal(popnbar$labels, popnbarkey$labels)
  
})

test_that("Q23 plot (visible)", {
  
  expect_equal(popnbar3$layers[[1]], popnbar3key$layers[[1]])
  expect_equal(popnbar3$scales, popnbar3key$scales)
  expect_equal(popnbar3$mapping, popnbar3key$mapping)
  expect_equal(popnbar3$labels, popnbar3key$labels)
  
})

test_that("Q25 plot (visible)", {
  
  expect_equal(popnbar5$layers[[1]], popnbar5key$layers[[1]])
  expect_equal(popnbar5$scales, popnbar5key$scales)
  expect_equal(popnbar5$mapping, popnbar5key$mapping)
  expect_equal(popnbar5$labels, popnbar5key$labels)
  
})
