import csv
import time
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from webdriver_manager.chrome import ChromeDriverManager

from bs4 import BeautifulSoup
from random import randint

driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()))

driver.get('https://www2.miamidadeclerk.gov/usermanagementservices')

soup = BeautifulSoup(driver.page_source, 'html.parser')

time.sleep(2)

username = driver.find_element(By.ID, 'userName')
username.send_keys('gvelasquezneira@ufl.edu')

time.sleep(2)

password = driver.find_element(By.ID, 'password')
password.send_keys('P@rtyr0ck202215')

time.sleep(6)

login = driver.find_element(By.XPATH, '//*[@id="content"]/form/div[2]/div[1]/div[2]/div[4]/input[1]')
login.click()

time.sleep(2)

# Add case list here
case_list = ["432025CT003813CTAXMX",
"432025CT003863CTAXMX",
"432025CT003865CTAXMX",
"432025CT003923CTAXMX",
"432025CT004153CTAXMX",
"432025CT004192CTAXMX",
"432025CT004206CTAXMX",
"432025CT004213CTAXMX"]

# This line has NA values to deal with empty cells
with open("case.csv", 'w', newline='') as myfile:
    c = csv.writer(myfile)
    c.writerow([
        'State Case No.',
        'Name',
        'Date of Birth',
        'Date Filed',
        'Date Closed',
        "NA",
        'Warrant Type',
        'Warrant Amount',
        'NA',
        'Assessment Amount',
        'Balance Due',
        'Stay Due Date',
        'Judge',
        'Defense Attorney',
        "NA",
        'File Section',
        'File Location',
        'Box No',
        'Defendant in Jail',
        'Defendant Release to',
        "NA",
        'Bond Amount',
        'Bond Status',
        'NA',
        'Arresting Agency',
        'Arrest Date',
        'NA',
        'Seq. No.',
        'Charge', 
        'Charge Type',
        "Result", 
        'Disposition',
        'Charge2', 
        'Charge Type2',
        "Result2", 
        'Disposition2'
        'Charge3', 
        'Charge Type3',
        "Result3", 
        'Disposition3',
        'Charge4', 
        'Charge Type4',
        "Result4", 
        'Disposition4',
        'Charge4', 
        'Charge Type4',
        "Result4", 
        'Disposition4',
        'Charge5', 
        'Charge Type5',
        "Result5", 
        'Disposition5',
        'Charge6', 
        'Charge Type6',
        "Result6", 
        'Disposition6',
        'Charge7', 
        'Charge Type7',
        "Result7", 
        'Disposition7',
        'Charge8', 
        'Charge Type8',
        "Result8", 
        'Disposition8'
    ])

# This send the keys to find each case. Make sure to uncomment case_list above
for case in case_list:
    driver.get('https://www2.miamidadeclerk.gov/cjis/CaseSearch.aspx')
    time.sleep(2)
    case_data = []
    case_number = driver.find_element(By.ID, 'ddCaseType')
    time.sleep(2)
    first_case = case[0]
    case_number.send_keys(first_case)

    time.sleep(2)

    case_number2 = driver.find_element(By.ID, 'txtCaseNo2')
    second_case = case[1:3]
    case_number2.send_keys(second_case)

    time.sleep(2)
    
    case_number3 = driver.find_element(By.ID, 'txtCaseNo3')
    third_case = case[3:9]
    case_number3.send_keys(third_case)

    time.sleep(2)

    case_search = driver.find_element(By.ID, "btnCaseSearch").click()

    time.sleep(3)

    soup = BeautifulSoup(driver.page_source, 'html.parser')
    case_info_table = soup.find('table', class_="table mt-0 mb-2 table-responsive-md")
    if case_info_table:
        case_rows = case_info_table.find('tbody')
        if case_rows:
            for row in case_rows:
                trows = case_rows.find_all('tr')
            if len(trows) < 10:
                for tr in trows:
                    tds = tr.find_all('td')
                    # Adds NA values to deal with empty cells
                    selected_tds = [tds[1].text.replace('\n', '').strip() if tds[1].text.replace('\n', '').strip() else "NA", 
                                        tds[3].text.replace('\n', '').strip() if tds[3].text.replace('\n', '').strip() else "NA", 
                                        tds[5].text.replace('\n', '').strip() if tds[5].text.replace('\n', '').strip() else "NA"]
                    case_data.extend(selected_tds)
            else:
                for idx, tr in enumerate(trows):
                    if "Previous Case" in tr.text and len(trows) >= 10 and idx == 4:  
                        continue
                    if "ALT Section" in tr.text and len(trows) >= 10 and idx == 6:
                        continue
                    tds = tr.find_all('td')
                    selected_tds = [
                        tds[1].text.replace('\n', '').strip() if tds[1].text.replace('\n', '').strip() else "NA",
                        tds[3].text.replace('\n', '').strip() if tds[3].text.replace('\n', '').strip() else "NA",
                        tds[5].text.replace('\n', '').strip() if tds[5].text.replace('\n', '').strip() else "NA"
                        ]
                    case_data.extend(selected_tds)
                    
    time.sleep(2)

    charges_div = soup.find('div', id='pnlCharges')

    if charges_div:
        charges_table = charges_div.find('table', class_='table table-bordered table-responsive-md')

        if charges_table:
            tbodies = charges_table.find_all('tbody')
            
            charges_body = tbodies[1] 
            rows = charges_body.find_all('tr')  
                
            for row in rows:
                cells = row.find_all('td')
                charge_data = [
                    cells[0].text.strip() if cells[0].text.strip() else "NA",
                    cells[1].text.strip() if cells[1].text.strip() else "NA",
                    cells[2].text.strip() if cells[2].text.strip() else "NA",
                    cells[3].text.strip() if cells[3].text.strip() else "NA"
                ]
                case_data.extend(charge_data)
                time.sleep(2)
    # Writes the data to the csv file
    with open("case.csv", 'a', newline='') as myfile:
        c = csv.writer(myfile)
        c.writerow(case_data)

driver.quit()