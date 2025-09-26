### Set Up fake data --------------------------------------
# These are from set.seed(2112); data.dad = fake_dad(); data.expl = fake_expl(dad = data.dad)
# then `dput()` to get them in code.
data.dad = structure(
  list(
    date = structure(
      c(
        18262, 18269, 18276, 18283, 
        18290, 18297, 18304, 18311, 18318, 18325, 18332, 18339, 18346, 
        18353, 18360, 18367, 18374, 18381, 18388, 18395, 18402, 18409, 
        18416, 18423, 18430, 18437, 18444, 18451, 18458, 18465, 18472, 
        18479, 18486, 18493, 18500, 18507, 18514, 18521, 18528, 18535, 
        18542, 18549, 18556, 18563, 18570, 18577, 18584, 18591, 18598, 
        18605, 18612, 18619, 18626), 
      class = "Date"),
    virus = c(
      "VXX", 
      "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", 
      "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", 
      "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", 
      "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", 
      "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", 
      "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX"), 
    geo = c(
      "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", 
      "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", 
      "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", 
      "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", 
      "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", 
      "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ"),
    count = c(
      57, 
      99, 141, 184, 254, 300, 356, 392, 421, 486, 497, 467, 585, 633, 
      599, 545, 647, 539, 553, 709, 550, 530, 747, 658, 786, 681, 669, 
      764, 797, 615, 621, 600, 703, 558, 681, 650, 610, 511, 496, 393, 
      415, 449, 408, 453, 423, 372, 250, 236, 195, 134, 99, 61, 0),
    percapita = c(
      5.7e-05, 9.9e-05, 0.000141, 0.000184, 0.000254, 
      3e-04, 0.000356, 0.000392, 0.000421, 0.000486, 0.000497, 
      0.000467, 0.000585, 0.000633, 0.000599, 0.000545, 0.000647, 
      0.000539, 0.000553, 0.000709, 0.00055, 0.00053, 0.000747, 
      0.000658, 0.000786, 0.000681, 0.000669, 0.000764, 0.000797, 
      0.000615, 0.000621, 6e-04, 0.000703, 0.000558, 0.000681, 
      0.00065, 0.00061, 0.000511, 0.000496, 0.000393, 0.000415, 
      0.000449, 0.000408, 0.000453, 0.000423, 0.000372, 0.00025, 
      0.000236, 0.000195, 0.000134, 9.9e-05, 6.1e-05, 0)
  ),
  class = "data.frame",
  row.names = c(NA, -53L)
)
data.expl = structure(
  list(
    date = structure(
      c(
        18262, 18269, 18276, 18283, 
        18290, 18297, 18304, 18311, 18318, 18325, 18332, 18339, 18346, 
        18353, 18360, 18367, 18374, 18381, 18388, 18395, 18402, 18409, 
        18416, 18423, 18430, 18437, 18444, 18451, 18458, 18465, 18472, 
        18479, 18486, 18493, 18500, 18507, 18514, 18521, 18528, 18535, 
        18542, 18549, 18556, 18563, 18570, 18577, 18584, 18591, 18598, 
        18605, 18612, 18619, 18626),
      class = "Date"),
    virus = c(
      "VXX", 
      "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", 
      "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", 
      "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", 
      "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", 
      "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", 
      "VXX", "VXX", "VXX", "VXX", "VXX", "VXX", "VXX"),
    geo = c(
      "GZZ", 
      "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", 
      "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", 
      "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", 
      "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", 
      "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", 
      "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ", "GZZ"),
    thecount = c(
      31, 
      49, 66, 98, 137, 133, 205, 171, 201, 225, 257, 230, 353, 284, 
      305, 256, 368, 251, 318, 359, 306, 219, 361, 359, 473, 339, 379, 
      382, 360, 290, 326, 259, 329, 253, 319, 302, 322, 238, 280, 196, 
      181, 237, 239, 214, 213, 183, 130, 111, 78, 70, 50, 35, 0)
    ),
  class = "data.frame",
  row.names = c(NA, -53L)
)

#### Testing with family = "lm" ---------------------------

prm = list(
  family = 'lm',
  data.dad = data.dad,
  data.expl = data.expl,
  varname.dad = 'count',
  varname.expl = 'thecount',
  date.range.fit = c('2020-01-10', '2020-12-15')
)

# from dput(fit_model(prm)$model$residuals)
original_residuals = c(`1` = -29.5007921930071, `2` = -43.6894976716715, `3` = -43.3882324737931, 
`4` = 9.7603557110399, `5` = -62.9142316159544, `6` = 33.8487679551263, 
`7` = 9.23435656887865, `8` = 31.3428274598806, `9` = -14.8458780187836, 
`10` = 3.40709222883929, `11` = -98.4119944547759, `12` = 72.9011517335936, 
`13` = 1.37106376322025, `14` = 34.9412690274247, `15` = -63.2192001478997, 
`16` = 37.877004258466, `17` = -67.861847837487, `18` = 14.8651232679746, 
`19` = -49.416083282988, `20` = 86.0657097371301, `21` = 49.290829175558, 
`22` = -36.1348767320254, `23` = -111.869639999766, `24` = 22.6080641921396, 
`25` = -60.8778176561905, `26` = 28.7607412051847, `27` = 101.077976221766, 
`28` = 44.1782694563441, `29` = -14.1590242071531, `30` = 84.5798278887999, 
`31` = 62.4795346542222, `32` = 53.3027101660495, `33` = 58.3510051163047, 
`34` = 57.732504901845, `35` = -18.0104360223201, `36` = 33.1099158591733, 
`37` = -56.9502600815734, `38` = -9.82990820008009, `39` = 38.9772974930437, 
`40` = -27.1029370946185, `41` = -71.677231187035, `42` = 18.0014449681713, 
`43` = -10.2114079856204, `44` = -7.59699659937278, `45` = -34.8782031503353, 
`46` = -14.9224092723785, `47` = 3.05344325249388, `48` = -43.6493803778401
)

# from dput(fitted.model(prm)$model$coefficients)
original_coefficients <- c(`(Intercept)` = 52.5490871432622, thecount.expl = 1.78714704620825)

# from dput(names(fitted.model))
output_names <- c("model", "data", "varname.dad", "varname.expl", "family", "date.range.fit")


test_that("Coefficients in fit_model() with family='lm' do not change", {
  expect_equal(fit_model(prm)$model$coefficients, original_coefficients)
})

test_that("Residuals in fit_model() with family='lm' do not change", {
  expect_equal(fit_model(prm)$model$residuals, original_residuals)
})

test_that("varname.dad in fit_model() with family='lm' does not change", {
  expect_equal(fit_model(prm)$varname.dad, "count")
})

test_that("varname.expl in fit_model() with family='lm' does not change", {
  expect_equal(fit_model(prm)$varname.expl, "thecount")
})

test_that("Output names in fit_model() with family='lm' does not change", {
  expect_equal(names(fit_model(prm)), output_names)
})


#### Testing with family = "log-lm"


prm = list(
  family = 'lm-log',
  data.dad = data.dad,
  data.expl = data.expl,
  varname.dad = 'count',
  varname.expl = 'thecount',
  date.range.fit = c('2020-01-10', '2020-12-15')
)

original_residuals_log = c(`1` = -0.0255533711735077, `2` = -0.127748333652422, `3` = -0.117530966042073, 
`4` = 0.0765295314568085, `5` = -0.155495755370708, `6` = 0.109822400983179, 
`7` = 0.0305684509542155, `8` = 0.0690361434202441, `9` = -0.032495392225842, 
`10` = 0.00867580768374735, `11` = -0.165234201206899, `12` = 0.116295249321371, 
`13` = -0.00538921738449069, `14` = 0.0633333111580199, `15` = -0.103278439540493, 
`16` = 0.0706433006553275, `17` = -0.124187724088884, `18` = 0.0113038265329963, 
`19` = -0.0937827743679129, `20` = 0.180891102542082, `21` = 0.0583365462610881, 
`22` = -0.0633467686738139, `23` = -0.142573040086629, `24` = 0.0244261789239744, 
`25` = -0.0972866005551702, `26` = 0.0281500738634994, `27` = 0.125710914253722, 
`28` = 0.0679652718106275, `29` = -0.0313673029924475, `30` = 0.148620583662721, 
`31` = 0.0841224478028102, `32` = 0.0978910453684887, `33` = 0.0810908431259499, 
`34` = 0.0855326266756855, `35` = -0.0377350026193051, `36` = 0.0668550332171657, 
`37` = -0.114381342203674, `38` = -0.0147813231264999, `39` = 0.113879001180315, 
`40` = -0.058568098814706, `41` = -0.162154501934597, `42` = 0.0454278847925761, 
`43` = -0.0187274325859303, `44` = -0.0057458403580875, `45` = -0.0845322454230741, 
`46` = 0.00507436414240416, `47` = 0.143017516147133, `48` = -0.131303781508985
)

original_coefficients_log <- c(`(Intercept)` = 1.0701977336304, `log(thecount.expl)` = 0.931846600331794)

test_that("Coefficients in fit_model() with family='lm-log' do not change", {
  expect_equal(fit_model(prm)$model$coefficients, original_coefficients_log)
})

test_that("Residuals in fit_model() with family='lm-log' do not change", {
  expect_equal(fit_model(prm)$model$residuals, original_residuals_log)
})

# The following should be the same as family="lm"
test_that("varname.dad in fit_model() with family='lm-log' does not change", {
  expect_equal(fit_model(prm)$varname.dad, "count")
})

test_that("varname.expl in fit_model() with family='lm-log' does not change", {
  expect_equal(fit_model(prm)$varname.expl, "thecount")
})

test_that("Output names in fit_model() with family='lm-log' does not change", {
  expect_equal(names(fit_model(prm)), output_names)
})