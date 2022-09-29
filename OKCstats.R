shot_stats <- function(){
shot_data = read.csv("C:/Users/zachc/OneDrive - onu.edu/Desktop/shots_data.csv", header=T)
df = as.data.frame(shot_data)

#create a data frame with only values of team A
teamA = subset(df, team == 'Team A')

#filter out the (x,y) for the right corner of team A
corner1A <- teamA %>%
  filter(x > 22, y <= 7.80)

#solve for shots attempted and shots made of the right corner of team A
corner_attempted1A=nrow(corner1A)
corner_made_1A=sum(corner1A$fgmade)

#filter out the (x,y) for the left corner of team A
corner2A <- teamA %>%
  filter(x < -22, y <= 7.80)

#solve for shots attempted and shots made of the left corner of team A
corner_attempted2A=nrow(corner2A)
corner_made_2A=sum(corner2A$fgmade)

#adds the attempts and makes from both corners of team A
corner_attempted_totalA = corner_attempted1A + corner_attempted2A
corner_made_totalA = corner_made_1A + corner_made_2A


#filter out the (x,y) for the non-corner threes for team A
nc3A <- teamA %>%
  filter((sqrt(x^2 + y^2)) > 23.75, y > 7.80)

#solve for shots attempted and shots made of non-corner threes for team A
nc3_attemptedA = nrow(nc3A)
nc3_madeA = sum(nc3A$fgmade)

#filter out the (x,y) for the 2pt shots of team A
two_pointA <- teamA %>%
  filter(sqrt(x^2 + y^2) < 23.75, x > -22, x < 22)

#solve for shots attempted and shots made of the 2pt shots of team A
two_point_attemptedA = nrow(two_pointA)
two_point_madeA = sum(two_pointA$fgmade)

#create a data frame with only the values of team B
teamB = subset(df, team == 'Team B')

#filter out the (x,y) for the right corner of team B
corner1B <- teamB %>%
  filter(x > 22, y <= 7.80)

#solve for shots attempted and shots made of the right corner of team B
corner_attempted1B=nrow(corner1B)
corner_made_1B=sum(corner1B$fgmade)

#filter out the (x,y) for the left corner of team B
corner2B <- teamB %>%
  filter(x < -22, y <= 7.80)

#solve for shots attempted and shots made of the left corner of team B
corner_attempted2B=nrow(corner2)
corner_made2B=sum(corner2$fgmade)

#adds the attempts and makes from both corners of team B
corner_attempted_totalB = corner_attempted1B + corner_attempted2B
corner_made_totalB = corner_made_1B + corner_made2B

#filter out the (x,y) for the non-corner threes for team B
nc3B <- teamB %>%
  filter((sqrt(x^2 + y^2)) > 23.75, y > 7.80)

#solve for shots attempted and shots made of non-corner threes for team B
nc3_attemptedB = nrow(nc3A)
nc3_madeB = sum(nc3A$fgmade)

#filter out the (x,y) for the 2pt shots of team B
two_pointB <- teamB %>%
  filter(sqrt(x^2 + y^2) < 23.75, x >= -22, x <= 22)

#solve for shots attempted and shots made of the 2pt shots of team A
two_point_attemptedB = nrow(two_pointB)
two_point_madeB = sum(two_pointB$fgmade) 

#prints the attempted and made numbers for each team and location
#stats = paste("TEAM A STATS", "C3 attempted:", corner_attempted_totalA, "C3 made:", corner_made_totalA, "NC3 attempted", nc3_attemptedA, 
#             "NC3 made", nc3_madeA, "2Pts attempted:", two_point_attemptedA, "2Pts made:", two_point_madeA, " ",
#             " ", "TEAM B STATS", "C3 attempted:", corner_attempted_totalB, "C3 made:", corner_made_totalB, 
#             "NC3 attempted", nc3_attemptedB, "NC3 made", nc3_madeB, "2Pts attempted:", two_point_attemptedB, "2Pts made:", two_point_madeB, sep="\n")

#cat(stats)

#total shots for team A
total_shotsA = two_point_attemptedA + nc3_attemptedA + corner_attempted_totalA
#total shots for team B
total_shotsB = two_point_attemptedB + nc3_attemptedB + corner_attempted_totalB

#percent of shots taken for team A
c3_percentA = corner_attempted_totalA/total_shotsA
nc3_percentA = nc3_attemptedA/total_shotsA
two_point_percentA = two_point_attemptedA/total_shotsA

#percent of shots taken for team B
c3_percentB = corner_attempted_totalB/total_shotsB
nc3_percentB = nc3_attemptedB/total_shotsB
two_point_percentB = two_point_attemptedB/total_shotsB

#prints all percentages
percentages = paste("", "", "TEAM A PERCENTAGES", "", "C3 PERCENTAGE:", c3_percentA, "", "NC3 PERCENTAGE:", nc3_percentA, "", "2 Pts PERCENTAGE:", two_point_percentA, " ", "",
            "TEAM B PERCENTAGES", "", "C3 PERCENTAGE:", c3_percentB, "", "NC3 PERCENTAGE:", nc3_percentB, "", "2 Pts PERCENTAGE:", two_point_percentB, sep="\n")
cat(percentages)

#eFG% for team A
c3_eFGA = (corner_made_totalA + 0.5 * corner_made_totalA)/corner_attempted_totalA
nc3_eFGA = (nc3_madeA + 0.5 * nc3_madeA)/nc3_attemptedA
two_point_eFGA = two_point_madeA/two_point_attemptedA

#eFG% for team B
c3_eFGB = (corner_made_totalB + 0.5 * corner_made_totalB)/corner_attempted_totalB
nc3_eFGB = (nc3_madeB + 0.5 * nc3_madeB)/nc3_attemptedB
two_point_eFGB = two_point_madeB/two_point_attemptedB

#prints all eFG%'s
eFG = paste("", "", "TEAM A eFG%'s", "", "C3 eFG%:", c3_eFGA, "", "NC3 eFG%:", nc3_eFGA, "", "2 Pts eFG%:", two_point_eFGA, " ", "",
                    "TEAM B eFG%'s", "", "C3 eFG%:", c3_eFGB, "", "NC3 eFG%:", nc3_eFGB, "", "2 Pts eFG%:", two_point_eFGB, sep="\n")
cat(eFG)
}