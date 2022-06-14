Survey summaries
================

**Cannot just re-run this! some denoms are hard-coded for non-response,
and there’s some text mining**

### Read files

    ## # A tibble: 2 × 2
    ##   survey  responses
    ##   <chr>       <int>
    ## 1 English        83
    ## 2 Spanish        99

## Summaries

### Demographics

| gender                           | English | Spanish |
|:---------------------------------|:--------|:--------|
| Man                              | 26%     | 34%     |
| Non-binary/Gender non-conforming | 1%      | 0%      |
| Woman                            | 72%     | 66%     |

Respondents by gender

![](survey_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

![](survey_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

| latino | English | Spanish |
|:-------|:--------|:--------|
| No     | 34%     | 0%      |
| Yes    | 66%     | 100%    |

Share of Latino respondents

Birthplace is an open text field, need to sort around for common words

![](survey_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

![](survey_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

| internet | English | Spanish |
|:---------|:--------|:--------|
| No       | 5%      | 13%     |
| Yes      | 95%     | 87%     |

Respondents with internet at home

| device | English | Spanish |
|:-------|:--------|:--------|
| No     | 5%      | 16%     |
| Yes    | 95%     | 84%     |

Respondents with smart devices

![](survey_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

![](survey_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

### News habits

![](survey_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->![](survey_files/figure-gfm/unnamed-chunk-26-2.png)<!-- -->

![](survey_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

Preferred sources is also an open text field. Mining for a word cloud…

![](survey_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

Denoms are hard-coded so we get share of respondents, not share of
responses

Also note that one person wrote in Puerto Rico for the geographical
question.

![](survey_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

We added Food in June. Also hard coded denoms.

![](survey_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

![](survey_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

### Trust in news media

![](survey_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

![](survey_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->
