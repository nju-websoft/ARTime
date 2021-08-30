# ARTime
Automatic Rule Generation for Time Expression Normalization (Findings of EMNLP, 2021)

The codes is obtained in the `software` directory.

The `evaluationResults.zip` file includes the main results of our paper.

## Citation ##
//TODO

## Building ##
ARTime is maven project built with IntelliJ IDEA. It runs with java-11 and scala-2.12.11. 

### Program Entry ###
First, mark `software/src/main` as Sources Root. The main function to reproduce the results of our method is `artime.Main.main()`. The train process and test process have been annotated. You need to choose the datasets from the commented codes for training and testing to reproduce all kinds of results. The code gives an example which treats Tweets-modified trainingset as trainingset and treats Tweets-modified testset as testset to get the results of ARTime normalization with gold mention on Tweets-modified.

### Switch to ARTime+H ###
If you want to reproduce the results of ARTime+H, you need to make 2 small modifications to our codes:

In artime.struct.TexToken.scala, line 76:

```
val On=true
```

In artime.rule.RuleHandler.scala, line 199:

```
val ruleSorted: IndexedSeq[Rule] = getRules(100)
```

## Evaluation tools ##
We use the standard evaluation tools of TempEval-3 Task to evaluate our performance， which is included in `software/evaluation_tools`.
