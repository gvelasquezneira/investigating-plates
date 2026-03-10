import pandas as pd
from playwright.sync_api import sync_playwright
import re

def run_palm_beach_scraper():
    all_data = []
    # Ensure this file exists in your directory
    plates = pd.read_csv('west-palm_records.csv')
    case_numbers = plates['court_docket_no'].tolist()

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=False)
        context = browser.new_context()
        page = context.new_page()

        print("Navigating to Palm Beach Clerk...")
        page.goto("https://appsgp.mypalmbeachclerk.com/eCaseView/VerifyUser.aspx")

        # 1. Click Guest Access
        try:
            page.locator("#cphBody_ibGuest").click(timeout=10000)
        except:
            print("Guest button not found, moving to captcha...")
        
        # 2. CAPTCHA PAUSE
        print("\n>>> Solve the CAPTCHA. DO NOT click Submit on the site.")
        input(">>> Press [ENTER] here once the CAPTCHA is ready...")

        # 3. CLICK SUBMIT
        page.locator("#cphBody_cmdContinue").click()
        page.wait_for_selector("#cphBody_gvSearch_txtParameter_0", timeout=15000)

        for case_no in case_numbers:
            try:
                print(f"\n--- Processing Case: {case_no} ---")
                
                # Search Logic
                page.locator("#cphBody_gvSearch_txtParameter_0").fill(case_no)
                page.locator("#cphBody_cmdSearch").click()
                
                # Open Case
                result_link = page.locator("#cphBody_gvResults_lbCaseNumber_0")
                result_link.wait_for(state="visible", timeout=10000)
                result_link.click()

                # --- STEP 4: Scrape Case Info (Main Table) ---
                page.wait_for_selector("#cphBody_dvCaseInfo", timeout=10000)
                case_details = {"case_number": case_no}
                
                info_rows = page.locator("#cphBody_dvCaseInfo tr")
                for i in range(info_rows.count()):
                    cells = info_rows.nth(i).locator("td")
                    if cells.count() >= 2:
                        label = cells.nth(0).inner_text().strip().lower().replace(":", "").replace(" ", "_")
                        value = cells.nth(1).inner_text().strip()
                        case_details[label] = value

                # --- STEP 5: Click Charges & Sentences Tab ---
                print("Navigating to Charges & Sentences...")
                page.locator("#cphBody_lbCharges").click()
                
                # Wait for the charges table to load
                page.wait_for_selector("#cphBody_gvCharges", timeout=10000)
                
                # Target all data rows (skipping the header row)
                # The site uses .datarow and .dataalternate for charge entries
                charge_rows = page.locator("#cphBody_gvCharges tr")
                
                statutes = []
                descriptions = []
                dispositions = []
                citation_numbers = []

                for j in range(charge_rows.count()):
                    cells = charge_rows.nth(j).locator("td")
                    # If it has 8+ cells, it's a data row, not a header
                    if cells.count() >= 8:
                        statutes.append(cells.nth(1).inner_text().strip())
                        descriptions.append(cells.nth(2).inner_text().strip())
                        dispositions.append(cells.nth(3).inner_text().strip())
                        citation_numbers.append(cells.nth(6).inner_text().strip())

                # Join them for the CSV
                case_details["charge_statutes"] = " | ".join(statutes)
                case_details["charge_descriptions"] = " | ".join(descriptions)
                case_details["all_dispositions"] = " | ".join(dispositions)
                case_details["citation_numbers"] = " | ".join(citation_numbers)
                
                all_data.append(case_details)
                print(f"Success: Fully scraped {case_no}")

            except Exception as e:
                print(f"Error on {case_no}: {e}")

            # RESET for next search
            page.goto("https://appsgp.mypalmbeachclerk.com/eCaseView/Search.aspx")

        browser.close()
    return all_data

if __name__ == "__main__":
    data = run_palm_beach_scraper()
    if data:
        df = pd.DataFrame(data)
        df.to_csv("palm_beach_full_details.csv", index=False)
        print("\nSUCCESS: Saved to palm_beach_full_details.csv")
        