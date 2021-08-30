# ARTime
Automatic Rule Generation for Time Expression Normalization (Findings of EMNLP, 2021)

//TODO
## Citation ##


## Building ##
ARTime is maven project built with IntelliJ IDEA. It runs with java-11 and scala-2.12.11. 

### Program Entry ###
First, mark 'software/src/main' as Sources Root. The main function to reproduce the results of our method is anonymous.ntime.Ttmp2.main(). The train process and test process have been annotated. You need to choose the datasets from the commented codes for training and testing to reproduce all kinds of results. The code gives an example which treats Tweets-modified trainingset as trainingset and treats Tweets-modified testset as testset to get the results of ARTime normalization with gold mention on Tweets-modified.

What's more, if you want to reproduce the results of ARTime+H, you need to make 2 small modifications to our codes:

In anonymous.ntime.struct.TexToken.scala, line 76:

```
val On=true
```

In anonymous.ntime.rule.RuleHandler.scala, line 199:

```
val ruleSorted: IndexedSeq[Rule] = getRules(100)
```

After making these 2 modifications, you can repeat switch ARTime to ARTime+H.

## Evaluation tools ##

We use the standard evaluation tools of TempEval-3 Task to evaluate our performance， which is included in 'software/evaluation_tools'.
