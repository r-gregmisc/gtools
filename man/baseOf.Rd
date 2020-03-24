\name{baseOf}
\alias{baseOf}
\title{Transform an integer to an array of base-n digits}
\description{
Transform an integer to an array of base-n digits
}
\usage{
baseOf(v, base=10, len=1)
}
\arguments{
  \item{v}{
    A single integer value to be transformed.
    }
  \item{base}{
    The base to which to transform to.
    }
  \item{len}{
    The minimal length of the returned array.
    }
}
\details{
 This function converts the elements of an integer vector as an array of its digits.
 The base of the numbering scheme may be changed away from 10,
 which defines our decimal system, to any other integer value. For
 base=2, the number is returned in the dual system. The least significant
 digit has the highest index in the array, i.e. it appears on the right.
 The highest exponent is at position 1, i.e. left.

 To write decimal values in another base is very common in computer science.
 In particular at the basis 2 the then possible values 0 and 1 are often
 interpreted as logical false or true. And at the very interface to
 electrical engineering, it is indicacted as an absence or presence of
 voltage. When several bit values are transported synchronously, then
 it is common to give every lane of such a data bus a unique 2^x value
 and interpret it as a number in the dual system. To distinguish 256
 characters one once needed 8 bit ("byte"). It is the common
 unit in which larger non-printable data is presented.
 Because of the many non-printable characters and the difficulty for most humans to
 memorize an even longer alphabet, it is presented as two half bytes ("nibble")
 of 4 bit in a hexadecimal presentation. Example code is shown below.

 For statisticians, it is more likely to use bit representations for
 hashing. A bit set to 1 (TRUE) at e.g. position 2, 9 or 17 is interpreted
 as the presence of a particular feature combination of a sample.
 With baseOf, you can refer to the bit combination as a number, which
 is more easily and more efficiently dealt with than with an array of
 binary values. The example code presents a counter of combinations of
 features which may be interpreted as a Venn diagram.
}
\author{Steffen Moeller \email{moeller@debian.org} }
\examples{
# decimal representation
baseOf(123)

# dual representation
baseOf(123,base=2)

# octal representation
baseOf(123,base=8)

# hexadecimal representation
baseOf(123,base=16)

# hexadecimal with more typical letter-notation
c(0:9,LETTERS)[baseOf(123,16)]

# hexadecimal again, now showing a single string
paste(c(0:9,LETTERS)[baseOf(123,16)],collapse="")

# decimal representation but filling leading zeroes
baseOf(123,len=5)

# and converting that back
sum(2^(4:0)*baseOf(123,len=5))

# hashing and a tabular venn diagram derived from it
m<-matrix(sample(c(FALSE,TRUE),replace=TRUE,size=300),ncol=4)
colnames(m)<-c("strong","colorful","nice","humorous")
names(dimnames(m)) <- c("samples","features")
head(m)

m.val <- apply(m,1,function(X){return(sum(2^((ncol(m)-1):0)*X))})
m.val.rle <- rle(sort(m.val))
m.counts <- cbind(baseOf(m.val.rle$value,base=2,len=ncol(m)),
                    m.val.rle$lengths)
colnames(m.counts)<- c(colnames(m),"num")
rownames(m.counts)<- apply(m.counts[,1:ncol(m)],1,paste,collapse="")
m.counts[1==m.counts[,"nice"]&1==m.counts[,"humorous"],,drop=FALSE]
m.counts[,"num",drop=TRUE]
}
\keyword{base}

