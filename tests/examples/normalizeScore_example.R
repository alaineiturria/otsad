## Generate data
set.seed(100)
n <- 180
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE)
x[25] <- 200
x[150] <- 170
df <- data.frame(timestamp = 1:n, value = x)

# Add is.real.anomaly column
df$is.real.anomaly <- 0
df[c(25,80,150), "is.real.anomaly"] <- 1

## Calculate anomalies
result <- CpSdEwma(
  data = df$value,
  n.train = 5,
  threshold = 0.01,
  l = 3
)
res <- cbind(df, result)

# Get null and perfect scores
np.scores <- GetNullAndPerfectScores(df)
np.standard <- np.scores[1,]
np.fp <- np.scores[2,]
np.fn <- np.scores[3,]

# Get detector score
scores <- GetDetectorScore(res, print = FALSE, title = "")

# Normalize standard score
NormalizeScore(scores$standard, np.standard$perfect.score, np.standard$null.score)

# Normalize low_FP_rate score
NormalizeScore(scores$low_FP_rate, np.fp$perfect.score, np.fp$null.score)

# Normalize low_FN_rate score
NormalizeScore(scores$low_FN_rate, np.fn$perfect.score, np.fn$null.score)
