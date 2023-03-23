#' @title Parent involvement survey: population data
#'
#' @description An example dataset describing a population of 20,000 students with disabilities
#' in 20 school districts. This population is the basis for selecting a sample of
#' students for a parent involvement survey.
#'
#' @format A data frame with 20,000 rows and 9 variables
#' @section Fields:
#'
#' \describe{
#' \item{UNIQUE_ID}{A unique identifier for students}
#' \item{SCHOOL_DISTRICT}{A unique identifier for school districts}
#' \item{SCHOOL_ID}{A unique identifier for schools, nested within districts}
#' \item{STUDENT_GRADE}{Student's grade level: 'PK', 'K', 1-12}
#' \item{STUDENT_AGE}{Student's age, measured in years}
#' \item{STUDENT_DISABILITY_CODE}{Code for student's disability category (e.g. 'VI' for 'Visual Impairments')}
#' \item{STUDENT_DISABILITY_CATEGORY}{Student's disability category (e.g. 'Visual Impairments')}
#' \item{STUDENT_SEX}{'Female' or 'Male'}
#' \item{STUDENT_RACE}{Seven-level code with descriptive label (e.g. 'AS7 (Asian)')}
#' }
#'
#' @examples
#' involvement_survey_pop
"involvement_survey_pop"
#'
#' @title Parent involvement survey: simple random sample
#'
#' @description An example dataset describing a simple random sample of 5,000 parents
#' of students with disabilities, from a population of 20,000.
#' The parent involvement survey measures a single key outcome:
#' whether "parents perceive that schools facilitate parent involvement
#' as a means of improving services and results for children with disabilities." \cr \cr
#' The variable \code{BASE_WEIGHT} provides the base sampling weight.
#' The variable \code{N_STUDENTS_IN_SCHOOL} can be used to provide a finite population correction
#' for variance estimation.
#' @format A data frame with 5,000 rows and 16 variables
#' @section Fields:
#'
#' \describe{
#' \item{UNIQUE_ID}{A unique identifier for students}
#' \item{RESPONSE_STATUS}{Survey response/eligibility status: 'Respondent', 'Nonrespondent', 'Ineligble', 'Unknown'}
#' \item{WHETHER_PARENT_AGREES}{Parent agreement ('AGREE' or 'DISAGREE') for whether they perceive that schools facilitate parent involvement}
#' \item{SCHOOL_DISTRICT}{A unique identifier for school districts}
#' \item{SCHOOL_ID}{A unique identifier for schools, nested within districts}
#' \item{STUDENT_GRADE}{Student's grade level: 'PK', 'K', 1-12}
#' \item{STUDENT_AGE}{Student's age, measured in years}
#' \item{STUDENT_DISABILITY_CODE}{Code for student's disability category (e.g. 'VI' for 'Visual Impairments')}
#' \item{STUDENT_DISABILITY_CATEGORY}{Student's disability category (e.g. 'Visual Impairments')}
#' \item{STUDENT_SEX}{'Female' or 'Male'}
#' \item{STUDENT_RACE}{Seven-level code with descriptive label (e.g. 'AS7 (Asian)')}
#' \item{PARENT_HAS_EMAIL}{Whether parent has an e-mail address ('Has Email' vs 'No Email')}
#' \item{PARENT_HAS_EMAIL_BENCHMARK}{Population benchmark for category of \code{PARENT_HAS_EMAIL}}
#' \item{PARENT_HAS_EMAIL_BENCHMARK}{Population benchmark for category of \code{STUDENT_RACE}}
#' \item{BASE_WEIGHT}{Sampling weight to use for weighted estimates}
#' \item{N_STUDENTS}{Total number of students in the population}
#' }
#'
#' @examples
#' involvement_survey_srs
"involvement_survey_srs"
#'
#' @title Parent involvement survey: stratified, two-stage sample
#'
#' @description An example dataset describing a stratified, multistage sample of 1,000 parents
#' of students with disabilities, from a population of 20,000.
#' The parent involvement survey measures a single key outcome:
#' whether "parents perceive that schools facilitate parent involvement
#' as a means of improving services and results for children with disabilities." \cr \cr
#' The sample was selected by sampling 5 schools from each of 20 districts,
#' and then sampling parents of 10 children in each sampled school.
#' The variable \code{BASE_WEIGHT} provides the base sampling weight.
#' The variable \code{SCHOOL_DISTRICT} was used for stratification,
#' and the variables \code{SCHOOL_ID} and \code{UNIQUE_ID} uniquely identify
#' the first and second stage sampling units (schools and parents).
#' The variables \code{N_SCHOOLS_IN_DISTRICT} and \code{N_STUDENTS_IN_SCHOOL}
#' can be used to provide finite population corrections.
#' @format A data frame with 5,000 rows and 16 variables
#' @section Fields:
#'
#' \describe{
#' \item{UNIQUE_ID}{A unique identifier for students}
#' \item{RESPONSE_STATUS}{Survey response/eligibility status: 'Respondent', 'Nonrespondent', 'Ineligble', 'Unknown'}
#' \item{WHETHER_PARENT_AGREES}{Parent agreement ('AGREE' or 'DISAGREE') for whether they perceive that schools facilitate parent involvement}
#' \item{SCHOOL_DISTRICT}{A unique identifier for school districts}
#' \item{SCHOOL_ID}{A unique identifier for schools, nested within districts}
#' \item{STUDENT_GRADE}{Student's grade level: 'PK', 'K', 1-12}
#' \item{STUDENT_AGE}{Student's age, measured in years}
#' \item{STUDENT_DISABILITY_CODE}{Code for student's disability category (e.g. 'VI' for 'Visual Impairments')}
#' \item{STUDENT_DISABILITY_CATEGORY}{Student's disability category (e.g. 'Visual Impairments')}
#' \item{STUDENT_SEX}{'Female' or 'Male'}
#' \item{STUDENT_RACE}{Seven-level code with descriptive label (e.g. 'AS7 (Asian)')}
#' \item{PARENT_HAS_EMAIL}{Whether parent has an e-mail address ('Has Email' vs 'No Email')}
#' \item{PARENT_HAS_EMAIL_BENCHMARK}{Population benchmark for category of \code{PARENT_HAS_EMAIL}}
#' \item{STUDENT_RACE_BENCHMARK}{Population benchmark for category of \code{STUDENT_RACE}}
#' \item{BASE_WEIGHT}{Sampling weight to use for weighted estimates}
#' \item{N_STUDENTS}{Total number of students in the population}
#' \item{JKn_Rep_Wt_1 - JKn_Rep_Wt_95}{Numeric variables containing JKn jackknife replicate weights which can be used for variance estimation}
#' \item{Boot_Rep_Wt_1 - Boot_Rep_Wt_50}{Numeric variables containing bootstrap replicate weights which can be used for variance estimation}
#' }
#'
#' @examples
#' # Load the data
#' involvement_survey_str2s
#'
#' # Prepare the data for analysis with the 'survey' package
#'
#'   library(survey)
#'
#'   ##_ Can specify survey design explicitly
#'
#'     involvement_survey <- svydesign(
#'       data = involvement_survey_str2s,
#'       weights = ~ BASE_WEIGHT,
#'       strata =  ~ SCHOOL_DISTRICT,
#'       ids =     ~ SCHOOL_ID             + UNIQUE_ID,
#'       fpc =     ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
#'     )
#'
#'   ##_ Can instead use provided replicate weights
#'     involvement_survey <- svrepdesign(
#'       data = involvement_survey_str2s,
#'       weights = ~ BASE_WEIGHT,
#'       repweights = "Boot_Rep_Wt"
#'     )
"involvement_survey_str2s"

#' @title Replicate-specific scale factors for the parent involvement survey stratified, two-stage sample
#'
#' @format A list with elements \code{'bootstrap'} and \code{'JKn'},
#' each a numeric vector of replicate-specific scale factors to supply to the
#' \code{svrepdesign} function from the 'survey' package.
#'
#' @examples
#' library(survey)
#' data('involvement_survey_str2s', package = 'nrba')
#'
#' # Using jackknife replicates
#' involvement_survey <- svrepdesign(
#'   data = involvement_survey_str2s,
#'   weights = ~ BASE_WEIGHT,
#'   repweights = "JKn_Rep_Wt",
#'   rscales = involvement_survey_str2s_rscales$JKn,
#'   type = "JKn",
#' )
#' # Using bootstrap replicates
#' involvement_survey <- svrepdesign(
#'   data = involvement_survey_str2s,
#'   weights = ~ BASE_WEIGHT,
#'   repweights = "Boot_Rep_Wt",
#'   rscales = involvement_survey_str2s_rscales$bootstrap,
#'   type = "bootstrap",
#' )
"involvement_survey_str2s_rscales"
