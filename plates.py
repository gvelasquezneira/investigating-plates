import pandas as pd
from playwright.sync_api import sync_playwright
import re

def clean_text(text):
    if not text: return ""
    text = text.replace('\xa0', ' ').replace('¬†', ' ')
    text = re.sub(r'\s+', ' ', text)
    return text.strip()

def run_scraper():
    all_data = []
    # plates = pd.read_csv('broward_plates.csv')
    # case_numbers = plates['court_docket_no'].tolist()
    case_numbers = ['062025CT163652A88840',
'062025CT165981A88810',
'062025CT166207A88830',
'062025CT167843A88810',
'062025CT167854A88810',
'062025CT171734A88830',
'062025CT178353A88830',
'062025CT179533A88840']  # testing

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=False)
        context = browser.new_context()
        page = context.new_page()

        print("Navigating to Broward Clerk...")
        page.goto("https://www.browardclerk.org/Web2")

        # initial agreement
        try:
            page.wait_for_selector("button:has-text('I Agree')", timeout=5000)
            page.click("button:has-text('I Agree')")
        except:
            pass

        for case_no in case_numbers:
            try:
                page.locator('a[href="#caseNumberSearch"]').click()
                page.wait_for_selector('input#CaseNumber', state="visible", timeout=10000)
                
                page.fill('input#CaseNumber', case_no)
                 
                input("Solve the captcha and press 'Enter' in the terminal")
                
                page.click('button#CaseNumberSearchResults')
                
                page.wait_for_selector('button.bc-casedetail-viewer', timeout=15000)
                page.click('button.bc-casedetail-viewer')
                page.wait_for_selector("#PartyDetailsRow tr", timeout=10000)
                current_parties = []
                for row in page.locator("#PartyDetailsRow tr").all():
                    cells = row.locator("td")
                    if cells.count() >= 4:
                        raw_details = cells.nth(1).inner_text().strip()
                        lines = raw_details.split('\n')
                        vitals = {line.split(":", 1)[0].strip().lower().replace(" ", "_"): line.split(":", 1)[1].strip() 
                                    for line in lines if ":" in line}
                        
                        current_parties.append({
                            "party_type": cells.nth(0).inner_text().strip(),
                            "full_name": lines[0] if lines else "N/A",
                            "address": cells.nth(2).inner_text().strip().replace('\n', ', '),
                            **vitals
                        })

                # Scraping charges
                page.evaluate("window.scrollTo(0, document.body.scrollHeight/2)")
                
                statutes, dates, degrees, agencies, descs = [], [], [], [], []
                
                if page.locator("#tblCharges").is_visible():
                    for crow in page.locator("#tblCharges tbody tr").all():
                        ccells = crow.locator("td")
                        if ccells.count() >= 4:
                            raw_text = clean_text(ccells.nth(2).inner_text())
                            degree = clean_text(ccells.nth(3).inner_text())
                            
                            # cleaning data
                            name_m = re.search(r'^(.*?)(?=Date Filed:|$)', raw_text)
                            date_m = re.search(r'Date Filed:\s*([\d/]+)', raw_text)
                            stat_m = re.search(r'Current Statute:\s*(.*?)(?=Filing Type:|$)', raw_text)
                            agen_m = re.search(r'Filing Agency:\s*(.*?)(?=Original Statute:|$)', raw_text)

                            descs.append(name_m.group(1).strip() if name_m else "N/A")
                            dates.append(date_m.group(1) if date_m else "N/A")
                            statutes.append(stat_m.group(1).strip() if stat_m else "N/A")
                            agencies.append(agen_m.group(1).strip() if agen_m else "N/A")
                            degrees.append(degree)

                # Scrape Dispo
                dispo_list = []
                if page.locator("#tblDispositionsCR").is_visible():
                    for drow in page.locator("#tblDispositionsCR tbody tr").all():
                        dcells = drow.locator("td")
                        if dcells.count() >= 3:
                            dispo_list.append(clean_text(dcells.nth(2).inner_text()))

                # Combine data
                for party in current_parties:
                    entry = {"case_number": case_no, **party}
                    if party['party_type'] == "Defendant":
                        entry["charge_descriptions"] = " | ".join(descs)
                        entry["charge_dates"] = " | ".join(dates)
                        entry["charge_statutes"] = " | ".join(statutes)
                        entry["charge_agencies"] = " | ".join(agencies)
                        entry["charge_degrees"] = " | ".join([f"({d})" for d in degrees])
                        entry["all_dispositions"] = " | ".join(dispo_list)
                    all_data.append(entry)

            except Exception as e:
                print(f"FAILED on case {case_no}: {e}")
            
            page.goto("https://www.browardclerk.org/Web2")
            try:
                page.wait_for_selector("button:has-text('I Agree')", timeout=3000)
                page.click("button:has-text('I Agree')")
            except:
                pass

        browser.close()
    return all_data

if __name__ == "__main__":
    data = run_scraper()
    if data:
        df = pd.DataFrame(data)
        df.to_csv("broward_partial_records.csv", index=False)