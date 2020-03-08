# Module UI
  
#' @title   mod_faq_ui and mod_faq_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_faq
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_faq_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$title("Frequently Asked Questions:"),
    tags$h2("General questions regarding the Data Depot on Education"),
    
    tags$h3("Why create a Data Depot on Education in Pakistan?"),
    tags$p("The Data Depot on Education aims to provide a platform for proactive and open access to
            standardized indicators relevant to education in Pakistan. Currently, information on education is
            scattered across multiple datasets in Pakistan. This data portal harmonizes the education relevant
            data for users through an easy-to-use online interface."),
    tags$p("Our hope is that it will allow policymakers and researchers to understand annual schooling trends
            and make informed policy decisions by visualizing data that are tailored to their interest—be it in
            comparisons of indicators over time, across districts, provinces, or by gender within the country."),
    tags$h3("Who created this Data Depot?"),
    tags$p("This platform is being led by Koen Geven at the World Bank. Ahmed Raza leads the cleaning of the
            data. Isabel Harbaugh Macdonald contributes to the narrative interpretation. Ayesha Tahir
            contributes to the publication of the dataset. Jishnu Das, Tahir Andrabi, Deon Filmer and others
            provided valuable advice in the design of the data portal. Alp Arslan Aziz and Tony Fujs created
            the functionality and data visualizations of the Data Depot using the R-programming language."),
    tags$h3("What are the sources for these indicators?"),
    tags$p("The Data Depot on Education in Pakistan harmonizes data from major household surveys in
            Pakistan. We sourced household datasets that are already publicly available, cleaned them, and 
            combined them to produce comparable estimates."),
    tags$h3("How were these indicators selected?"),
    tags$p("We have given priority to data and indicators that are of the most relevance to the main issues facing
            education in Pakistan. This includes data on school enrolment, learning outcomes, and public-
            private sector dynamics. Over time, more data points will be added to the depot."),
    tags$h3("Why is this limited to education indicators?"),
    tags$p("We are exploring ways to expand the Data Depot towards a broader set of indicators that are
            relevant for human development. However, we are a small team with limited bandwidth, and
            therefore currently are prioritizing indicators that we use on a daily basis."),
    tags$h3("What are the sources of these data points and how can I learn more?"),
    tags$p("Sources of data that we bring together are produced by 6 differences sources:"),
    tags$ul(
      tags$li(tags$p(tags$a("ASER", href = "http://aserpakistan.org/index.php"),": The Annual Status of Education Report is a citizen-led; household-based survey that
                     aims to provide reliable estimates on the schooling status of children and basic learning
                     levels of children aged 5-16 years residing in all rural and several urban districts of Pakistan.
                     It’s a large household-based survey covering all i.e. 138 districts and major urban cities of the
                     country and is implemented each year. The results are representative at both provincial and
                     district levels. "),
              tags$p("The Annual Status of Education Report is used to understand the broad patterns in learning
                     across districts and over time. It gives an overview of the learning competencies (grades 2/3-
                     lower primary) of Pakistan&#39;s children, whether in school or out of school. The ASER tool is
                     mapped to lower primary competencies of grades 2-3 or SDG 4.1.1a. Each indicator included in ASER measures both the public and private schools’ outcomes. ASER data also
                     provides educational indicators including enrollment levels, school facilities, mothers’ literacy
                     and various other school elements, apart from the learning levels of the children.")),
      
      tags$li(tags$p(tags$a("HIES", href = "http://www.pbs.gov.pk/content/household-integrated-economic-survey-hies-2015-16"),":Household Integrated Economic Survey implemented by the Pakistan Bureau of
                    Statistics provides information on household income, savings, liabilities, and consumption
                    expenditure, and social indicators at national and provincial levels. HIES data is used by
                    Planning Commission for estimation of consumption-based Poverty. The HIES is
                    implemented in urban and rural areas of all four provinces of Pakistan."),
                    tags$p("HIES is split into two modules in order to obtain better quality of information
                    independently from male and female respondents by male and female enumerators
                    respectively.")),
              
      tags$li(tags$p(tags$a("PSLM", href ="http://www.pbs.gov.pk/content/pakistan-social-and-living-standards-measurement"),": The Pakistan Social and Living Standards Measurement (PSLM) is a household
                    survey that provides social and economic indicators, which is representative at provincial and
                    district levels. It includes survey modules on education, health, employment, household
                    assets and amenities, income and expenditure, population welfare, water supply and
                    sanitation."),
              tags$p("The survey is carried out in alternate years and is the primary source of estimation of Multi-
                    Dimensional Poverty in Pakistan. The sample size of PSLM surveys district level was
                    approximately 80000 households and approximately 26000 at Provincial level. The PSLM
                    consists of all urban and rural areas of the four provinces of Pakistan and Islamabad. PSLM
                    surveys are instrumental in monitoring 29 indicators of SDGS in Pakistan.")),
      
      tags$li(tags$p(tags$a("MICS", href ="http://www.bos.gop.pk/mics"), ": Multiple Indicator Cluster Survey Punjab is a household survey implemented by the
                    Bureau of Statistics Punjab in collaboration with the United Children’s Fund (UNICEF). It
                    provides data for assessing the situation of children, adolescents, women and households in
                    Punjab. MICS surveys cover both rural and urban areas for all 36 districts of Punjab. The
                    results from the MICS Punjab survey are representative at both the district and tehsil/town
                    level as well. The MICS Punjab survey covers all household members, women aged 15-49,
                    men aged 15-49 years, all children under five and children age 5-17 years living in the
                    household."),
              tags$p("MICS Punjab allows the provincial government and districts to gauge and monitor their
              respective status of human and social development with precise data on a variety of key
              indicators. It allows to track the progress of Track the progress of World Summit for
              Children (WSC), World Fit for Children (WFFC), SDGs and PRSP indicators.")),
      tags$li(tags$p(tags$a("DHS",  href ="https://dhsprogram.com/what-we-do/survey/survey-display-523.cfm"),": The Pakistan Demographic and Health Survey is a household survey implemented by
              the National Institute of Population Studies. DHS provides internationally standardized
              indicators of demographic and health indicators. The results from this survey are
              representative at the provincial and district levels. There are two main types of DHS Surveys:"),
              tags$ul(
                tags$li("Standard DHS Surveys have large sample sizes (usually between 5,000 and 30,000
                households) and typically are conducted about every 5 years, to allow comparisons
                over time."),
                tags$li("Interim DHS Surveys focus on the collection of information on key performance
                monitoring indicators but may not include data for all impact evaluation measures
                (such as mortality rates). These surveys are conducted between rounds of DHS
                surveys and have shorter questionnaires than DHS surveys. Although nationally
                representative, these surveys generally have smaller samples than DHS surveys.")
                )),
      
      tags$li(tags$p(tags$a("EGRA", href ="https://www.usaid.gov/news-information/videos/early-grade-reading-assessment-egra-extra-mile-better-journey"),": EGRA is an individually administered assessment tool that measures the
              foundational literacy skills that readers need before they can read, such as letter recognition,
              and oral reading fluency, as well as basic reading comprehension."),
              tags$p("The Pakistan EGRA survey was administered as part of a USAID funded reading project,
              providing baseline, midline and end-line data. Students at two selected grade levels-grade 3
              and 5-were assessed. EGRA was administered to a sample of 33,000 children in 11,20
              schools throughout the country. In all provinces, except Sindh, students were tested in Urdu.
              In Sindh, the test was administered in Sindhi. EGRA assessment was carried out in
              government schools in Pakistan, and a random sample of both male and female schools was
              chosen."))
              ),
   tags$h2("Specific questions about data"),
   tags$h3("How did you calculate these indicators?"),
   tags$p("After data cleaning and aggregation of the data on key indicators from different sources, we
          estimated means and standard errors using survey weights to correct for the sampling design. We
          carefully followed the documentation of each individual survey."),
    tags$h3("Why is the district the lowest level of aggregation available?"),
    tags$p("None of the surveys currently included are representative at lower levels than districts. As we are
          adding administrative datasets to the portal, we are trying to make these available at Tehsil and
          Union Council level. Unfortunately, it is proving difficult to present Union Council-level data (let
          alone at lower levels of administration), since the coding of these districts is not consistent between
          datasets, and across time."),
   tags$h3("How can I download these indicators?"),
   tags$p("We are working to ensure that very soon, it will be possible to download specific indicators from the
          portal or to download the entire data underlying the portal."),
   tags$h3("How can I access the microdata?"),
   tags$p("We are trying to find out the best way to publish the microdata. Micro-datasets have been made
          available to use in various ways, mostly because these are already available in the public domain.
          However, we do not hold the licenses to publish the microdata here directly. We are planning to
          work with the rights holders to try and find ways to make the microdata more easily accessible."),
   tags$h3("I have a question about the data, can you help me?"),
   tags$p("As we are a small team, we currently do not have the bandwidth to offer any advice about the
          interpretation of the data. We sometimes make some exceptions to this rule for researchers and
          journalists who are writing in-depth stories, but please do not expect a reply. We do write blogs and
          narratives that put the data into context and provides an interpretation of indicators."),
   tags$h3("Is the data up to date? Are there plans to add more data in the future?"),
   tags$p("Yes, we have integrated the main datasets that we know are available and are actively planning to
          add more data as they become available. If you would like to contribute a relevant dataset to this
          portal, please email us at", a("Data_Depot_Pakistan@worldbank.org.", 
                                         href = "mailto:Data_Depot_Pakistan@worldbank.org")
          ),
   tags$p("We also plan to add several waves of administrative data to the portal. We will make these available
          as a specific tab on the Punjab province, which we expect to add during 2020."),
   tags$h3("What can I do if I spot an error in the data?"),
   tags$p("We strive to reduce errors as much as possible, although we are sure to have made some errors.
          Please don’t hesitate to get in touch with our team at", a("Data_Depot_Pakistan@worldbank.org.", 
                                                                     href = "mailto:Data_Depot_Pakistan@worldbank.org")
   )
  )
}
    
# Module Server
    
#' @rdname mod_faq
#' @export
#' @keywords internal
    
mod_faq_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_faq_ui("faq_ui_1")
    
## To be copied in the server
# callModule(mod_faq_server, "faq_ui_1")
 
