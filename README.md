## Effects of human-animal interaction on positive youth development: A replication study

-   Created on 2024-11-07 by Jeffrey R. Stevens
    (<jeffrey.r.stevens@gmail.com>)
-   Finalized on 2022-10-27

This repository provides the reproducible research materials for our
project that investigates how human-animal interaction, animal
attitudes, and positive are related. Materials are available at [Open
Science Framework](https://osf.io/8tkhp/):

-   Data
-   R script for data analysis
-   Quarto file for the manuscript

## Citation

If you use any of these materials, please cite:

> Pachunka, A., Jeffries, J., Karr, L., Luck, L., Reiling, B.A.,
> Schultz, D.H., & Stevens, J.R. (2024). Effects of human-animal
> interaction on positive youth development: A replication study.
> *People and Animals*.
> [doi:10.31234/osf.io/ge7bf](https://doi.org/10.31234/osf.io/ge7bf)

## Summary

This study conducted two survey-based studies replicating [Mueller
(2014)](https://doi.org/10.1080/10888691.2014.864205). Undergraduate
students from University of Nebraska-Lincoln’s College of Agricultural
Science and Natural Resources and Department of Psychology participated
from March-May 2022 (Study 1) and June 2023 and January-March 2024
(Study 2). Participants answered questions about their animal experience
and interactions, animal attitudes, positive youth development, and
well-being.

## License

All materials presented here are released under the Creative Commons
Attribution 4.0 International Public License (CC BY 4.0). You are free
to:

-   Share — copy and redistribute the material in any medium or format
-   Adapt — remix, transform, and build upon the material for any
    purpose, even commercially. Under the following terms:
-   Attribution — You must give appropriate credit, provide a link to
    the license, and indicate if changes were made. You may do so in any
    reasonable manner, but not in any way that suggests the licensor
    endorses you or your use.

No additional restrictions — You may not apply legal terms or
technological measures that legally restrict others from doing anything
the license permits.

## Files

### Data files

`pachunka_etal_2024_data.csv`

| Variable | Description |
|:----------------|:------------------------------------------------------|
| source | Sample source (CASNR = College of Agricultural Sciences and Natural Resources, SONA = SONA study pool, REP = Study 2) |
| id | Participant ID |
| date | Date of survey completion |
| ownership | Whether participant currently owned animal |
| chosen_animal | Animal species chosen to answer questions about |
| companion_livestock | Whether chosen animal was considered pet/companion animal or livestock |
| animal_experience - Dog | Amount of experience with dogs (Study 1) or presence/absence of experience with dogs (Study 2) |
| animal_experience - Cat | Amount of experience with cats (Study 1) or presence/absence of experience with cats (Study 2) |
| animal_experience - Fish | Amount of experience with fish (Study 1) |
| animal_experience - Bird | Amount of experience with birds (Study 1) |
| animal_experience - Rabbit | Amount of experience with rabbits (Study 1) |
| animal_experience - Ferret | Amount of experience with ferrets (Study 1) |
| animal_experience - Small rodent (e.g., hamster, gerbil, chinchilla) | Amount of experience with small rodents (Study 1) |
| animal_experience - Reptile | Amount of experience with reptiles (Study 1) |
| animal_experience - Horse | Amount of experience with horses (Study 1) or presence/absence of experience with horses (Study 2) |
| animal_experience - Cow | Amount of experience with cows (Study 1) |
| animal_experience - Pig | Amount of experience with pigs (Study 1) |
| animal_experience - Goat | Amount of experience with goats (Study 1) |
| animal_experience - Sheep | Amount of experience with sheep (Study 1) |
| animal_experience - Llama | Amount of experience with llamas (Study 1) |
| animal_experience - Poultry | Amount of experience with poultry (Study 1) |
| animal_experience - Small animal | Presence/absence of experience with small animals (Study 2) |
| animal_experience - Large animal | Presence/absence of experience with large animals (Study 2) |
| animal_experience - Other | Presence/absence of experience with other animals (Study 2) |
| animal_activities - Feeding | Time spent feeding the chosen animal |
| animal_activities - Cleaning | Time spent cleaning the chosen animal |
| animal_activities - Grooming | Time spent grooming the chosen animal |
| animal_activities - Training | Time spent training the chosen animal |
| animal_activities - Petting/playing | Time spent petting/playing with the chosen animal |
| animal_activities - Engaging in therapy sessions | Time spent engaging in therapy sessions with the chosen animal |
| animal_activities - Riding/handling | Time spent riding/handling the chosen animal |
| animal_activities - Horseback riding | Time spent horseback riding |
| animal_activities - Dog showing or livestock competitions | Time spent showing dogs or in livestock competitions |
| animal_activities - Animal-related club or extracurricular activity | Time spent in animal-related club or extracurricular activities |
| animal_activities - Volunteering in an animal shelter or animal therapy program | Time spent volunteering in an animal shelter or animal therapy program |
| activities_score | Combined score for animal activities |
| care_score | Combined score for animal care |
| ccas1 | Comfort from Companion Animals Scale question 1 - My animal provides me with companionship |
| ccas2 | Comfort from Companion Animals Scale question 2 - My animal makes me feel needed |
| ccas3 | Comfort from Companion Animals Scale question 3 - My animal makes me feel loved |
| ccas4 | Comfort from Companion Animals Scale question 4 - Having an animal gives me something to care for |
| ccas5 | Comfort from Companion Animals Scale question 5 - My animal provides me with pleasurable activity |
| ccas6 | Comfort from Companion Animals Scale question 6 - My animal is a source of constancy in my life |
| ccas7 | Comfort from Companion Animals Scale question 7 - My animal makes me play and laugh |
| ccas8 | Comfort from Companion Animals Scale question 8 - Having an animal gives me something to love |
| ccas9 | Comfort from Companion Animals Scale question 9 - I get comfort from touching my animal |
| ccas10 | Comfort from Companion Animals Scale question 10 - I enjoy watching my animal |
| ccas11 | Comfort from Companion Animals Scale question 11 - My animal makes me feel trusted |
| laps1 | Lexington Attachment to Pets Scale question 1 - My animal means more to me than any of my friends |
| laps2 | Lexington Attachment to Pets Scale question 2 - I believe my animal is my best friend |
| laps3 | Lexington Attachment to Pets Scale question 3 - I enjoy showing other people pictures of my animal |
| laps4 | Lexington Attachment to Pets Scale question 4 - I love my animal because s/he never judges me |
| laps5 | Lexington Attachment to Pets Scale question 5 - I often talk to other people about my animal |
| laps6 | Lexington Attachment to Pets Scale question 6 - My animal and I have a very close relationship |
| laps7 | Lexington Attachment to Pets Scale question 7 - I would do almost anything to take care of my animal |
| laps8 | Lexington Attachment to Pets Scale question 8 - I play with my animal quite often |
| laps9 | Lexington Attachment to Pets Scale question 9 - I consider my animal to be a great companion |
| laps10 | Lexington Attachment to Pets Scale question 10 - My animal makes me feel happy |
| laps11 | Lexington Attachment to Pets Scale question 11 - I feel that my animal is a part of my family |
| attach1 | Additional attachment question 1 - I confide in my animal |
| attach2 | Additional attachment question 2 - My animal understands me |
| attach3 | Additional attachment question 3 - My animal knows when I’m feeling bad |
| mrcps1 | Miller-Rada Commitment to Pets Scale question 1 - If an animal destroyed a $4000 worth of furniture or personal items, I would get rid of the animal |
| mrcps2 | Miller-Rada Commitment to Pets Scale question 2 - If an adult animal was having problems with destructiveness, I would get rid of it |
| mrcps3 | Miller-Rada Commitment to Pets Scale question 3 - If a young animal required extensive veterinary care, I would get rid of it |
| mrcps4 | Miller-Rada Commitment to Pets Scale question 4 - If an adult animal was having problems with house breaking, I would get rid of it |
| aas1 | Animal Attitude Scale question 1 - I think it is perfectly acceptable for cattle and hogs to be raised for human consumption |
| aas2 | Animal Attitude Scale question 2 - The use of animals such as rabbits for testing the safety of cosmetics and household products is unnecessary and should be stopped |
| aas3 | Animal Attitude Scale question 3 - It is morally wrong to hunt wild animals just for sport |
| aas4 | Animal Attitude Scale question 4 - Basically, humans have the right to use animals as we see fit |
| aas5 | Animal Attitude Scale question 5 - I do not think that there is anything wrong with using animals in medical research |
| aas6 | Animal Attitude Scale question 6 - I sometimes get upset when I see wild animals in cages at zoos |
| aas7 | Animal Attitude Scale question 7 - Breeding animals for their skins is a legitimate use of animals |
| aas8 | Animal Attitude Scale question 8 - Some aspects of biology can only be learned through dissecting preserved animals such as cats |
| aas9 | Animal Attitude Scale question 9 - The slaughter of whales and dolphins should be immediately stopped even if it means some people will be put out of work |
| aas10 | Animal Attitude Scale question 10 - It is unethical to breed purebred dogs for pets when millions of dogs are killed in animal shelters each year |
| animaluse1 | Additional animal use question 1 - Much of the scientific research done with animals is unnecessary and cruel |
| animaluse2 | Additional animal use question 2 - I believe that animals should have the same rights and privileges as humans |
| caring1_1 | PYD Caring component 1 question 1 |
| caring1_2 | PYD Caring component 1 question 2 |
| caring2_1 | PYD Caring component 2 question 1 |
| caring2_2 | PYD Caring component 2 question 2 |
| caring3_1 | PYD Caring component 3 question 1 |
| caring3_2 | PYD Caring component 3 question 2 |
| character_conbeh1 | PYD Character Conduct Behavior component question 1 |
| character_conbeh2 | PYD Character Conduct Behavior component question 2 |
| character_perval1 | PYD Character Personal Values component question 1 |
| character_perval2 | PYD Character Personal Values component question 2 |
| character_soccon1 | PYD Character Social Conscience component question 1 |
| character_soccon2 | PYD Character Social Conscience component question 2 |
| character_valdiv1 | PYD Character Values Diversity component question 1 |
| character_valdiv2 | PYD Character Values Diversity component question 2 |
| competence_accomp1 | PYD Competence Academic component question 1 |
| competence_accomp2 | PYD Competence Academic component question 2 |
| competence_physcomp1 | PYD Competence Physical component question 1 |
| competence_physcomp2 | PYD Competence Physical component question 2 |
| competence_soccomp1 | PYD Competence Social component question 1 |
| competence_soccomp2 | PYD Competence Social component question 2 |
| confidence_appear1 | PYD Confidence Physical Appearance component question 1 |
| confidence_appear2 | PYD Confidence Physical Appearance component question 2 |
| confidence_posid1 | PYD Confidence Positive Identity component question 1 |
| confidence_posid2 | PYD Confidence Positive Identity component question 2 |
| confidence_selfworth1 | PYD Confidence Self-worth component question 1 |
| confidence_selfworth2 | PYD Confidence Self-worth component question 2 |
| connection_confam1 | PYD Connection Family component question 1 |
| connection_confam2 | PYD Connection Family component question 2 |
| connection_conneigh1 | PYD Connection Neighborhood component question 1 |
| connection_conneigh2 | PYD Connection Neighborhood component question 2 |
| connection_conpeer1 | PYD Connection Peer connection component question 1 |
| connection_conpeer2 | PYD Connection Peer connection component question 2 |
| connection_consch1 | PYD Connection School component question 1 |
| connection_consch2 | PYD Connection School component question 2 |
| contribution1_1 | PYD Contribution component 1 question 1 |
| contribution1_2 | PYD Contribution component 1 question 2 |
| contribution1_3 | PYD Contribution component 1 question 3 |
| contribution1_4 | PYD Contribution component 1 question 4 |
| contribution2_1 | PYD Contribution component 2 question 1 |
| contribution2_2 | PYD Contribution component 2 question 2 |
| contribution2_3 | PYD Contribution component 2 question 3 |
| contribution2_4 | PYD Contribution component 2 question 4 |
| contribution3_1 | PYD Contribution component 3 question 1 |
| contribution3_2 | PYD Contribution component 3 question 2 |
| contribution3_3 | PYD Contribution component 3 question 3 |
| contribution3_4 | PYD Contribution component 3 question 4 |
| cesd1 | Center for Epidemiological Studies Depression scale question 1 - I was bothered by things that usually don’t bother me |
| cesd2 | Center for Epidemiological Studies Depression scale question 2 - I did not feel like eating; my appetite was poor |
| cesd3 | Center for Epidemiological Studies Depression scale question 3 - I felt that I could not shake off the blues even with help from my family or friends |
| cesd4 | Center for Epidemiological Studies Depression scale question 4 - I felt I was just as good as other people |
| cesd5 | Center for Epidemiological Studies Depression scale question 5 - I had trouble keeping my mind on what I was doing |
| cesd6 | Center for Epidemiological Studies Depression scale question 6 - I felt depressed |
| cesd7 | Center for Epidemiological Studies Depression scale question 7 - I felt that everything I did was an effort |
| cesd8 | Center for Epidemiological Studies Depression scale question 8 - I felt hopeful about the future |
| cesd9 | Center for Epidemiological Studies Depression scale question 9 - I thought my life had been a failure |
| cesd10 | Center for Epidemiological Studies Depression scale question 10 - I felt fearful |
| cesd11 | Center for Epidemiological Studies Depression scale question 11 - My sleep was restless |
| cesd12 | Center for Epidemiological Studies Depression scale question 12 - I was happy |
| cesd13 | Center for Epidemiological Studies Depression scale question 13 - I talked less than usual |
| cesd14 | Center for Epidemiological Studies Depression scale question 14 - I felt lonely |
| cesd15 | Center for Epidemiological Studies Depression scale question 15 - People were unfriendly |
| cesd16 | Center for Epidemiological Studies Depression scale question 16 - I enjoyed life |
| cesd17 | Center for Epidemiological Studies Depression scale question 17 - I had crying spells |
| cesd18 | Center for Epidemiological Studies Depression scale question 18 - I felt sad |
| cesd19 | Center for Epidemiological Studies Depression scale question 19 - I felt that people disliked me |
| cesd20 | Center for Epidemiological Studies Depression scale question 20 - I could not get going |
| stait1 | State-Train Anxiety Inventory – Trait scale question 1 - I feel pleasant |
| stait2 | State-Train Anxiety Inventory – Trait scale question 2 - I feel nervous and restless |
| stait3 | State-Train Anxiety Inventory – Trait scale question 3 - I feel satisfied with my life |
| stait4 | State-Train Anxiety Inventory – Trait scale question 4 - I wish I could be as happy as other seem to be |
| stait5 | State-Train Anxiety Inventory – Trait scale question 5 - I feel like a failure |
| stait6 | State-Train Anxiety Inventory – Trait scale question 6 - I feel rested |
| stait7 | State-Train Anxiety Inventory – Trait scale question 7 - I am “calm, cool, and collected” |
| stait8 | State-Train Anxiety Inventory – Trait scale question 8 - I feel that difficulties are piling up so that I cannot overcome them |
| stait9 | State-Train Anxiety Inventory – Trait scale question 9 - I worry too much over something that really doesn’t matter |
| stait10 | State-Train Anxiety Inventory – Trait scale question 10 - I am happy |
| stait11 | State-Train Anxiety Inventory – Trait scale question 11 - I have disturbing thoughts |
| stait12 | State-Train Anxiety Inventory – Trait scale question 12 - I lack self-confidence |
| stait13 | State-Train Anxiety Inventory – Trait scale question 13 - I feel secure |
| stait14 | State-Train Anxiety Inventory – Trait scale question 14 - I make decisions easily |
| stait15 | State-Train Anxiety Inventory – Trait scale question 15 - I feel inadequate |
| stait16 | State-Train Anxiety Inventory – Trait scale question 16 - I am content |
| stait17 | State-Train Anxiety Inventory – Trait scale question 17 - Some unimportant thought runs through my mind and bothers me |
| stait18 | State-Train Anxiety Inventory – Trait scale question 18 - I take disappointments so keenly that I can’t put them out of my mind |
| stait19 | State-Train Anxiety Inventory – Trait scale question 19 - I am a steady person |
| stait20 | State-Train Anxiety Inventory – Trait scale question 20 - I get in a state of tension or turmoil as I think over my recent concerns and interests |
| isr1 | Intentional Self Regulation question 1 - I concentrate all my energy on few things : I divide my energy among many things |
| isr2 | Intentional Self Regulation question 2 - I take things as they come and carry on from there : I consider exactly what is important for me |
| isr3 | Intentional Self Regulation question 3 - When I do not succeed right away at what I want to do, I don’t try other possibilities for very long : I keep trying as many different possibilities as are necessary to succeed at my goal |
| isr4 | Intentional Self Regulation question 4 - When something does not work as well as before, I get advice from experts or read books : When something does not work as well as before, I am the one who knows what is best for me |
| isr5 | Intentional Self Regulation question 5 - Even if something is important to me, it can happen that I don’t invest the necessary time or effort : For important things, I pay attention to whether I need to devote more time or effort |
| isr6 | Intentional Self Regulation question 6 - When I want to achieve something difficult, I wait for the right moment and the best opportunity : When I want to achieve something difficult, I don’t want to wait long for the very best opportunity |
| isr7 | Intentional Self Regulation question 7 - I don’t think long about how to realize my plans, I just try it : I think about exactly how I can best realize my plans |
| isr8 | Intentional Self Regulation question 8 - I make every effort to achieve a given goal : I prefer to wait for a while and see if things will work out by themselves |
| isr9 | Intentional Self Regulation question 9 - Even in difficult situations, I don’t burden others : When things aren’t going so well, I accept help from others |
| isr10 | Intentional Self Regulation question 10 - When I have started something that is important to me, but has little chance at success, I make a particular effort : When I start something that is important to me, but has little chance at success, I usually stop trying |
| isr11 | Intentional Self Regulation question 11 - I am always working on several goals at once : I always focus on the one most important goal at a given time |
| isr12 | Intentional Self Regulation question 12 - Even when I really consider what I want in life, I wait and see what happens instead of committing myself to just one or two particular goals : When I think about what I want in life, I commit myself to one or two important goals |
| isr13 | Intentional Self Regulation question 13 - When I decide upon a goal, I stick to it : I can change a goal again at any time |
| isr14 | Intentional Self Regulation question 14 - When I want to get ahead, I don’t have a tendency to look at how others have done it : When I want to get ahead, I also look at how others have done it |
| isr15 | Intentional Self Regulation question 15 - When things don’t work the way they used to, I look for other ways to achieve them : When things don’t work the way they used to, I accept things the way they are |
| isr16 | Intentional Self Regulation question 16 - When I can’t do something as well as I used to, then I ask someone else to do it for me : When I can’t do something as well as I used to, I accept the change |
| isr17 | Intentional Self Regulation question 17 - When something doesn’t work as well as usual, I look at how others do it : When something doesn’t work as well as usual, I don’t spend much time thinking about it |
| isr18 | Intentional Self Regulation question 18 - I always pursue goals one after the other : I always pursue many goals at once, so that I easily get bogged down |
| esa | Whether participant has a prescribed emotional support animal |
| showanimals_yesno | Whether participant has ever shown animals in competitions |
| showanimals_frequency | Frequency of showing animals in competitions |
| showanimals_level | Level at which showing animals (local, state, national) |
| ffa4h_membership | Membership in 4-H and/or FFA |
| ffa4h_years | Number of years participating in 4-H and/or FFA |
| ffa4h_animal | Whether participant has participated in animal-based projects in 4-H and/or FFA |
| ffa4h_responsibility | Degree to which 4-H and/or FFA taught participant responsibility |
| ffa4h_compassion | Degree to which 4-H and/or FFA taught participant compassion |
| ffa4h_respect | Degree to which 4-H and/or FFA taught participant respect |
| gender | Participant gender identity |
| race - Latina/o/x or Hispanic or heritage from a Latin American country | Participant race/ethnicity - Latina/o/x or Hispanic or heritage from a Latin American country |
| race - African American/Black | Participant race/ethnicity - African American/Black |
| race - Native American/American Indian/Indigenous | Participant race/ethnicity - Native American/American Indian/Indigenous |
| race - Middle Eastern/Arab/Turkish/Iranian | Participant race/ethnicity - Middle Eastern/Arab/Turkish/Iranian |
| race - Asian/Asian American/Pacific Islander | Participant race/ethnicity - Asian/Asian American/Pacific Islander |
| race - White/European American | Participant race/ethnicity - White/European American |
| race - Biracial/multiracial | Participant race/ethnicity - Biracial/multiracial |
| race - I would prefer not to answer | Participant race/ethnicity - I would prefer not to answer |
| rural_urban | Location where participant grew up (rural, suburban, urban: small to medium city, urban: big city |
| relationship | Participant relationship status (married, partnered without legal recognition, in a long-term relationship, dating someone, single never married, divorced/separated, widowed, I would prefer not to answer) |
| income | Annual parental income |
| class_rank | Participant university class rank/year (freshman, sophomore, junior, senior, graduate student) |
| major | Participant university major |

### R code

`pachunka_etal_2024_rcode.R` - code for running computations and
generating figures

### Quarto document

`pachunka_etal_2024.qmd` - Quarto document with R code embedded for main
manuscript and appendix

### Installation

To reproduce these results, first clone or unzip the Git repository into
a folder. Then, ensure that a subfolder named “figures” is in the
folder. Next, open `pachunka_etal_2024_rcode.R` in
[RStudio](https://rstudio.com) or another R interface and ensure that
all packages mentioned at the top of the script are installed. Once all
packages are installed, run the script in R using
`source("pachunka_etal_2024_rcode.R")`.

Once the script runs without errors, you can compile the Quarto document
`pachunka_etal_2024.qmd.` Open this file in RStudio and ensure that you
have [{knitr}](https://yihui.org/knitr/) and
[Quarto](https://quarto.org/) installed. Once installed, render the
document (control-shift-K).

# Dataset Metadata

The following table is necessary for this dataset to be indexed by
search engines such as <a href="https://g.co/datasetsearch">Google
Dataset Search</a>.

<table>
<tr>
<th>
property
</th>
<th>
value
</th>
</tr>
<tr>
<td>
name
</td>
<td>
<code itemprop="name">Human-animal interaction and positive youth
development dataset</code>
</td>
</tr>
<tr>
<td>
description
</td>
<td>
<code itemprop="description">The dataset from the paper
<a href="https://doi.org/10.31234/osf.io/ge7bf">Effects of human-animal
interaction on positive youth development: A replication study</a>. This
study conducted two survey-based studies replicating [Mueller
(2014)](https://doi.org/10.1080/10888691.2014.864205). Undergraduate
students from University of Nebraska-Lincoln’s College of Agricultural
Science and Natural Resources and Department of Psychology participated
from March-May 2022 (Study 1) and June 2023 and January-March 2024
(Study 2). Participants answered questions about their animal experience
and interactions, animal attitudes, positive youth development, and
well-being.</code>
</td>
</tr>
</tr>
<tr>
<td>
url
</td>
<td>
<code itemprop="url"><https://github.com/unl-cchil/haipyd></code>
</td>
</tr>
<tr>
<td>
sameAs
</td>
<td>
<code itemprop="sameAs"><https://github.com/unl-cchil/haipyd></code>
</td>
</tr>
<tr>
<td>
citation
</td>
<td>
<code itemprop="citation"><https://doi.org/10.31234/osf.io/ge7bf></code>
</td>
</tr>
<tr>
<td>
license
</td>
<td>

<table>
<tr>
<th>
property
</th>
<th>
value
</th>
</tr>
<tr>
<td>
name
</td>
<td>
<code itemprop="name">CC BY-SA 4.0</code>
</td>
</tr>
<tr>
<td>
url
</td>
<td>
<code itemprop="url"><https://creativecommons.org/licenses/by-sa/4.0/></code>
</td>
</tr>
</table>

</td>
</tr>
</table>
