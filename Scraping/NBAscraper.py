import xlsxwriter
import datetime
import requests
from urllib.request import Request, urlopen
from bs4 import BeautifulSoup


def getListOfTeams():

    # get a list of all teams
    result = requests.get(
        "https://www.teamrankings.com/nba/stat/shooting-pct?date=2019-10-24")
    src = result.content

    soup = BeautifulSoup(src, 'html.parser')
    links = soup.find_all('trackValue')

    stat_table = soup.find(class_="tr-table datatable scrollable")
    stat_table_data = stat_table.tbody.find_all("tr")

    list_of_teams = []
    for i in range(0, len(stat_table_data)):
        headings = []
        for td in stat_table_data[i].find_all("td"):
            # remove any newlines and extra spaces from left and right
            headings.append(td.text)
        list_of_teams.append(headings[1])
    return list_of_teams


def createDayDict(list_of_teams):
    day_data = {}
    for team in list_of_teams:
        day_data[team] = {}
    return day_data


def getDataForStatOnDay(statID, date):
    data_url = "https://www.teamrankings.com/nba/stat/" + \
        statID + "?date=" + str(date)
    result = requests.get(data_url)

    src = result.content

    soup = BeautifulSoup(src, 'html.parser')
    links = soup.find_all('trackValue')

    stat_table = soup.find(class_="tr-table datatable scrollable")
    stat_table_data = stat_table.tbody.find_all("tr")
    final = []
    for i in range(0, len(stat_table_data)):
        headings = []
        for td in stat_table_data[i].find_all("td"):
            # remove any newlines and extra spaces from left and right
            headings.append(td.text)
        final.append([headings[1], headings[2]])
    return final


def writeArrayToExcel(array, excelFilename):
    workbook = xlsxwriter.Workbook(excelFilename)
    worksheet = workbook.add_worksheet()

    col = 0

    for row, data in enumerate(array):
        worksheet.write_row(row, col, data)

    workbook.close()


def formatDate(date):
    month = date.month
    day = date.day
    if day < 10:
        day = "0" + str(day)
    formatted_date = str(month) + str(day)
    return formatted_date


selected_features = ['points-per-game', 'average-scoring-margin', 'offensive-efficiency', 'floor-percentage', '1st-quarter-points-per-game', '2nd-quarter-points-per-game', '3rd-quarter-points-per-game', '4th-quarter-points-per-game', '1st-half-points-per-game', '2nd-half-points-per-game', 'overtime-points-per-game', 'points-in-paint-per-game', 'fastbreak-points-per-game', 'fastbreak-efficiency', 'average-biggest-lead', 'average-1st-quarter-margin', 'average-2nd-quarter-margin', 'average-3rd-quarter-margin', 'average-4th-quarter-margin', 'average-1st-half-margin', 'average-2nd-half-margin', 'average-overtime-margin', 'average-margin-thru-3-quarters', 'points-from-2-pointers', 'points-from-3-pointers', 'percent-of-points-from-2-pointers', 'percent-of-points-from-3-pointers', 'percent-of-points-from-free-throws', 'shooting-pct', 'effective-field-goal-pct', 'three-point-pct', 'two-point-pct', 'free-throw-pct', 'true-shooting-percentage', 'field-goals-made-per-game', 'field-goals-attempted-per-game', 'three-pointers-made-per-game', 'three-pointers-attempted-per-game', 'free-throws-made-per-game', 'free-throws-attempted-per-game', 'three-point-rate', 'two-point-rate', 'fta-per-fga', 'ftm-per-100-possessions', 'free-throw-rate', 'non-blocked-2-pt-pct', 'offensive-rebounds-per-game', 'defensive-rebounds-per-game', 'team-rebounds-per-game', 'total-rebounds-per-game', 'offensive-rebounding-pct', 'defensive-rebounding-pct', 'total-rebounding-percentage', 'blocks-per-game', 'steals-per-game', 'block-pct', 'steal-pct', 'assists-per-game', 'turnovers-per-game', 'turnovers-per-possession', 'assist--per--turnover-ratio', 'assists-per-fgm', 'assists-per-possession', 'turnover-pct', 'personal-fouls-per-game', 'technical-fouls-per-game', 'personal-fouls-per-possession', 'personal-foul-pct', 'opponent-points-per-game', 'opponent-average-scoring-margin', 'defensive-efficiency', 'opponent-floor-percentage', 'opponent-1st-quarter-points-per-game', 'opponent-2nd-quarter-points-per-game', 'opponent-3rd-quarter-points-per-game',
                     'opponent-4th-quarter-points-per-game', 'opponent-overtime-points-per-game', 'opponent-points-in-paint-per-game', 'opponent-fastbreak-points-per-game', 'opponent-fastbreak-efficiency', 'opponent-average-biggest-lead', 'opponent-1st-half-points-per-game', 'opponent-2nd-half-points-per-game', 'opponent-points-from-2-pointers', 'opponent-points-from-3-pointers', 'opponent-percent-of-points-from-2-pointers', 'opponent-percent-of-points-from-3-pointers', 'opponent-percent-of-points-from-free-throws', 'opponent-shooting-pct', 'opponent-effective-field-goal-pct', 'opponent-three-point-pct', 'opponent-two-point-pct', 'opponent-free-throw-pct', 'opponent-true-shooting-percentage', 'opponent-field-goals-made-per-game', 'opponent-field-goals-attempted-per-game', 'opponent-three-pointers-made-per-game', 'opponent-three-pointers-attempted-per-game', 'opponent-free-throws-made-per-game', 'opponent-free-throws-attempted-per-game', 'opponent-three-point-rate', 'opponent-two-point-rate', 'opponent-fta-per-fga', 'opponent-ftm-per-100-possessions', 'opponent-free-throw-rate', 'opponent-non-blocked-2-pt-pct', 'opponent-offensive-rebounds-per-game', 'opponent-defensive-rebounds-per-game', 'opponent-team-rebounds-per-game', 'opponent-total-rebounds-per-game', 'opponent-offensive-rebounding-pct', 'opponent-defensive-rebounding-pct', 'opponent-blocks-per-game', 'opponent-steals-per-game', 'opponent-block-pct', 'opponent-steals-perpossession', 'opponent-steal-pct', 'opponent-assists-per-game', 'opponent-turnovers-per-game', 'opponent-assist--per--turnover-ratio', 'opponent-assists-per-fgm', 'opponent-assists-per-possession', 'opponent-turnovers-per-possession', 'opponent-turnover-pct', 'opponent-personal-fouls-per-game', 'opponent-technical-fouls-per-game', 'opponent-personal-fouls-per-possession', 'opponent-personal-foul-pct', 'games-played', 'possessions-per-game', 'extra-chances-per-game', 'effective-possession-ratio', 'opponent-effective-possession-ratio', 'win-pct-all-games', 'win-pct-close-games', 'opponent-win-pct-all-games', 'opponent-win-pct-close-games']
# selected_features = ['points-per-game','average-scoring-margin']
list_of_teams = getListOfTeams()
# print(list_of_teams)

# create master array for data
master_array = []
header = ['date', 'team']
header.extend(selected_features)
master_array.append(header)
# print("master_array")
# print(master_array)

start_date = datetime.date(2019, 10, 22)
end_date = datetime.date(2020, 2, 1)
current_date = start_date

for i in range(0, (end_date - start_date).days):  # TODO: date in date range
    # create data dictionary for specific date
    day_data = createDayDict(list_of_teams)
    for feature in selected_features:
        stats_on_day = getDataForStatOnDay(
            feature, current_date)  # TODO: replace with data
        for teamstat in stats_on_day:
            day_data[teamstat[0]][feature] = teamstat[1]
    # Add the team stats for the day to the master_array
    for team in day_data:
        formatted_date = formatDate(current_date)
        row_to_add = [formatted_date, team]
        for stat in day_data[team]:
            row_to_add.append(day_data[team][stat])
        master_array.append(row_to_add)
    current_date += datetime.timedelta(days=1)
print(master_array)


writeArrayToExcel(master_array, 'testing.xlsx')
