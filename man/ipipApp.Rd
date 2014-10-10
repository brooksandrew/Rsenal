\name{ipipApp}
\alias{ipipApp}
\title{IPIP-NEO visualization in Shiny}
\source{
  \link{http://www.personal.psu.edu/j5j/IPIP/}
}
\usage{
  ipipApp(ipip35)
}
\arguments{
  \item{ipip35}{data.frame of ipip-neo results.  Should be
  35 rows. First column should be the personality traits
  from the IPIP-NEO Personality test. Each additional
  column should be the corresponding results from the test
  for a person. Frist row is treated is as a header
  (Person's names). \cr}
}
\value{
  Shiny App
}
\description{
  The IPIP-NEO (International Personality Item Pool) is a
  comprehensive personality test of 5 human dimensions:
  Extraversion, Agreeableness, Conscientiousness,
  Neuroticism, Openness to New Experience. \cr This is a
  wrapper around a relatively large application built in
  \code{\link{shiny}}. The \code{ui.R} and \code{server.R }
  components are built into the \code{shinyApp} function
  which allows the user to call the shiny app as a function
  with whatever input dataset is desired.
}
\examples{
## creating some fake ipip data
ipipdf <- data.frame(
 X=c('Extraversion','Friendliness','Gregariousness','Assertiveness','Activity Level','Excitement Seeking','Cheerfulness',
     'Agreeableness','Trust','Morality','Altruism','Cooperation','Modesty','Sympathy',
     'Conscientiousness','Self Efficacy','Orderliness','Dutifulness','Achievement-Striving','Self Discipline','Cautiousness',
     'Neuroticism','Anxiety','Anger','Depression','Self Consciousness','Immoderation','Vulnerability',
     'Openness to Experience','Imagination','Artistic Interests','Emotionality','Adventurousness','Intellect','Liberalism'),
 Baelish=runif(35, 0, 100),
 Tyrion=runif(35, 0, 100),
 Cersei=runif(35, 0, 100),
 Ned=runif(35, 0, 100),
 Robert=runif(35, 0, 100),
 John=runif(35, 0, 100),
 Varys=runif(35, 0, 100),
 Arya=runif(35, 0, 100),
 stringsAsFactors=F
)

## actually run the Shiny App
ipipApp(ipipdf)
}

