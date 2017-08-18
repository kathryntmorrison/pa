

ds = fromJSON(file = 'rover')
ds = unlist(ds)

N = length(ds)
seq_price = seq(1, N, by=4)
seq_name  = seq(2, N, by=4)
seq_repeats  = seq(3, N, by=4)
seq_reviews  = seq(4, N, by=4)

price = lapply(X = ds[seq_price], FUN = gsub, pattern = '\\$', replacement = '')
  
# sstrsplit(x = ds[seq_price], split = '\\$')

df = data.frame(
  price = as.numeric(price),
  name = ds[seq_name],
  repeats = as.numeric(ds[seq_repeats]),
  reviews = as.numeric(ds[seq_reviews]))

df = df %>% mutate(name = as.character(name))
class(df$name)


df[21, 'price'] = 35
df[21, 'name'] = c('Kathryn')
df[21, 'repeats'] = c(6)
df[21, 'reviews'] = c(12)

df$me = ifelse(df$name == 'Kathryn', 'me', 'not me')

p = df %>% group_by(price) %>% summarise(n = n()) 

ggplot(data = p) + geom_col(aes(y=n, x=as.factor(price), fill=as.factor(price)), position='dodge')


ggplot(data = df) + geom_point(aes(x = reviews, y = price, colour=me), size=2)


ggplot(data = df) + geom_point(aes(x = repeats, y = price, colour=me), size=2)

cor(df$repeats, df$price)
cor(df$reviews, df$price)


m = lm(price ~ reviews + repeats, data = df)


