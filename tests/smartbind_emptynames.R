library(gtools)

df1 <- data.frame(a=1, b=2,          d=TRUE )
df2 <- data.frame(     b=7, c="YES", d=FALSE)
df3 <- data.frame(     b=7, c="YES"         )

smartbind( df1, df2, df3 )
smartbind( df1=df1, df2, df3=df3 )

