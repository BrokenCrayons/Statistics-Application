library(wordcloud)

wordcloud(myfile.freq$word,myfile.freq$freq.Freq,random.order=FALSE,random.color=FALSE,colors=mycolors,family ="GB18030 Bitmap")
head(myfile.freq$word)
head(myfile.freq$freq.Freq)

function (words, freq, scale = c(4, 0.5), min.freq = 3, max.words = Inf, 
          random.order = TRUE, random.color = FALSE, rot.per = 0.1, 
          colors = "black", ordered.colors = FALSE, use.r.layout = FALSE, 
          fixed.asp = TRUE, ...) 
{
  if (!fixed.asp && rot.per > 0) 
    stop("Variable aspect ratio not supported for rotated words. Set rot.per=0.")
  tails <- "g|j|p|q|y"
  last <- 1
  nc <- length(colors)
  if (missing(freq)) { # 如果没有指明词频这一列的话，说明用户直接把需要分析的材料全部放进来了
    requireNamespace("tm") #在函数中要求有条件地调用tm (text mining)
    requireNamespace("slam") #在函数中要求有条件地调用slam (Sparse Lightweight Arrays and Matrices)
    if (is.character(words) || is.factor(words)) { # 判断给的关键词列表是否为character类型或factor类型
      corpus <- tm::Corpus(tm::VectorSource(words)) # 先将我们的关键词表转换为tm这个包可以处理的语料库corpus的形式
      corpus <- tm::tm_map(corpus, tm::removePunctuation) # 使用tm这个包里的函数removePunctuation,删去所有的标点符号 
      corpus <- tm::tm_map(corpus, function(x) tm::removeWords(x, tm::stopwords())) 
      # 使用tm这个包里的函数，自定义一个参数，遍历corpus，删除所有非检索用词
    }# 如果是character类型或factor类型
    else corpus <- words # 如果用户给出的words不是character也不是factor类型的话，直接将words赋值给corpus
    tdm <- tm::TermDocumentMatrix(corpus) # 使用tm包中的corpus函数把corpus变成一个词项文档矩阵
    freq <- slam::row_sums(tdm) # 然后使用slam这个包中的row_sums函数将上面的这个矩阵中有值的行数进行计数，就得到了词频
    words <- names(freq) # 将freq这个表的名字赋值给words，其实就是生成了一个关键词表。
  }
  
  
  if (ordered.colors) { # 如果按照词频分配颜色的这个按钮被置为TRUE的话
    if (length(colors) != 1 && length(colors) != length(words)) {# 先判断颜色的个数是否与关键词表的长度相同
      stop(paste("Length of colors does not match length of words", # 如果不同就跳出函数并报错
                 "vector"))
    }
  }
  if (min.freq > max(freq)) 
    min.freq <- 0      # 如果允许的最小词频比给出的词表中最大的词频还大的话，就自动把最小词频改为0
  
  ################################################## 写一个名叫overlap的函数 服务于下面调整词语在图上的位置
  overlap <- function(x1, y1, sw1, sh1) { 
    if (!use.r.layout) # 如果用的是c++而不是r的话
      return(is_overlap(x1, y1, sw1, sh1, boxes)) # 返回 is_overlap(x1, y1, sw1, sh1, boxes) 也就是一个直接的判断是否重叠了的logical值
    s <- 0
    if (length(boxes) == 0) # 如果boxes里啥都没有，就返回FALSE（也就是当屏幕上没有东西的时候，新加入的词是不会和别的东西重叠到一起去的）
      return(FALSE)
    for (i in c(last, 1:length(boxes))) { # boxes为空，则走一次这个流程（last控制）；boxes不为空，则for循环遍历boxes里面所有已经存在了的元素
      bnds <- boxes[[i]]
      x2 <- bnds[1] # 将boxes里第i个元素的四个定位点搬出来，和x1, y1, sw1, sh1分别比较
      y2 <- bnds[2]
      sw2 <- bnds[3]
      sh2 <- bnds[4]
      if (x1 < x2) # 判断x方向上是否有冲突
        overlap <- x1 + sw1 > x2 - s
      else overlap <- x2 + sw2 > x1 - s
      if (y1 < y2) # 判断y方向上是否有冲突
        overlap <- overlap && (y1 + sh1 > y2 - s)
      else overlap <- overlap && (y2 + sh2 > y1 - s)
      if (overlap) { # x或y方向上有至少一个方向上有冲突
        last <<- i # 则全局赋值 把i给last，下一次调用last的时候，last就变成i而不是1了
        return(TRUE) # 然后返回TRUE表示真的重叠了
      }
    }
    FALSE # 否则返回FALSE表示没有重叠
  }
  ##################################################
  ord <- rank(-freq, ties.method = "random") # 按照词频的倒序进行排列，当有遇到词频一样的情况时，采用随机的排列方式
  words <- words[ord <= max.words] # 选出关键词列表中的top x个词（如果有规定最大词数的话），并按顺序排列
  freq <- freq[ord <= max.words] # 同样把词频列也缩减到top x个词的词频，并按顺序排列
  if (ordered.colors) { # 如果字体是要求按词频顺序着色的
    colors <- colors[ord <= max.words] # 把颜色也缩减到要求的个数，并按顺序排列
  }
  if (random.order) # 如果词云的排列方式时随机的话
    ord <- sample.int(length(words)) # 按关键词表的个数产生随机排列的一列数字作为次序
  else ord <- order(freq, decreasing = TRUE) # 如果词云的方式不是随机排列，则按照词频的降序排列
  words <- words[ord] # 重新排列
  freq <- freq[ord] # 重新排列
  
  words <- words[freq >= min.freq] # 将大于最小词频的词重新排列
  freq <- freq[freq >= min.freq] # 将大于最小词频的筛选出来
  
  if (ordered.colors) { # 如果安顺序着色
    colors <- colors[ord][freq >= min.freq] # 那么着色的列表也需要修改到对应的长度
  }
  
  thetaStep <- 0.1 
  rStep <- 0.05
  
  plot.new() # 开始画图
  op <- par("mar")
  par(mar = c(0, 0, 0, 0)) # 将四条边的边框都设置为0
  if (fixed.asp) # 如果指定了图形得比例的话
    plot.window(c(0, 1), c(0, 1), asp = 1) # 就将图形设置为1:1的形式
  else plot.window(c(0, 1), c(0, 1)) # 如果没有指定图形的比例，就不设置asp
  normedFreq <- freq/max(freq) # 根据最大的词频，将所有的频率调整为标准化的词频（好像也不是标准化，就是成比例的吧）
  size <- (scale[1] - scale[2]) * normedFreq + scale[2] # 用这个公式调整size，使得词频从0-max的词字号match最小字号—最大字号
  
  boxes <- list() # 一开始是个空的list，下面的for循环每走一格，就会增加一个元素
  for (i in 1:length(words)) { # 遍历关键词表
    rotWord <- runif(1) < rot.per # 随机判断正在遍历的这个词要不要被旋转(runif(1) -> 生成1个0-1之间均匀分布随机数的函数) 
    r <- 0
    theta <- runif(1, 0, 2 * pi) # 生成1个0-2pi间均匀分布的随机数当成theta
    x1 <- 0.5
    y1 <- 0.5
    wid <- strwidth(words[i], cex = size[i], ...) # 根据之前调整的参数计算这个词的占地面积 - 宽
    ht <- strheight(words[i], cex = size[i], ...) # 根据之前调整的参数计算这个词的占地面积 - 高
    if (grepl(tails, words[i]))  # 判断词语中是否有"g|j|p|q|y"这几个字母
      ht <- ht + ht * 0.2 # 如果有的话要对高度再做一些调整（当然对中文字就不适用了）
    if (rotWord) { # 如果根据上面的runif这个词要被旋转的话
      tmp <- ht # 那就宽变成高，高变成宽
      ht <- wid
      wid <- tmp
    }
    ################################################## 下面这一部分都是为了调整图形的位置
    isOverlaped <- TRUE
    while (isOverlaped) { 
      if (!overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht) && x1 - 0.5 * wid > 0 && y1 - 0.5 * ht > 
          0 && x1 + 0.5 * wid < 1 && y1 + 0.5 * ht < 1) { # 如果overlap返回FALSE表示没有重叠，而且也没有超出框框的限制
        if (!random.color) { # 如果不是随机填色的话
          if (ordered.colors) { # 如果字体颜色顺序需要控制
            cc <- colors[i] # 就按之前调好的颜色列表相应的填色
          }
          else { # 如果字体颜色不需要控制
            cc <- ceiling(nc * normedFreq[i]) # 就按照词频排列来填色（向上取整：词表长度*标准化的频率）
            cc <- colors[cc] # 调取排名为cc的颜色
          }
        }
        else { # 如果是随机填色的话
          cc <- colors[sample(1:nc, 1)] # 那就在colors 1-nc之间随机抽一个颜色
        }
        text(x1, y1, words[i], cex = size[i], offset = 0, 
             srt = rotWord * 90, col = cc, ...) # 在调整好的跟别的地方没有重叠的x1,y1上将词打印出来，字号也是调好的，颜色也是调好的，是不是要旋转也是根据rotword确定的。
        boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht) # 然后把它的各项指标放到boxes里占坑，以便之后的词不要碰到它
        isOverlaped <- FALSE # 然后将isOverlaped置为FALSE退出while循环
      }
      else { # 如果overlap返回TRUE表示有重叠
        if (r > sqrt(0.5)) { # 判断r已经到了边缘，没办法继续了，就祭出警告，然后中止画图
          warning(paste(words[i], "could not be fit on page. It will not be plotted."))
          isOverlaped <- FALSE
        }
        theta <- theta + thetaStep # 调整角度
        r <- r + rStep * thetaStep/(2 * pi) # 调整半径
        x1 <- 0.5 + r * cos(theta) # 重新设置x1
        y1 <- 0.5 + r * sin(theta) # 重新设置y1 然后回到 while里再试一遍和boxes里其他已经有了的词有没有冲突
      }
    }
  }
  par(mar = op) # 将一开始调整了的环境参数调回来
  invisible() # 返回一张看不见的图 （返回对象的临时不可见副本的函数）
}
<bytecode: 0x10f034e18>
<environment: namespace:wordcloud>