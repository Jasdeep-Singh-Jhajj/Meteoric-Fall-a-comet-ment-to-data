import sys
import re

# /Users/jeremiah/UA/spring_24/dataviz/project_2/orbitals

orbital_file = sys.argv[1]

is_data = False
dateline = False
filename = orbital_file.split("/")[-1].split(".")[0]
date_val = ""
dataline = False

csv_text = "x,y,z,year,month,day,body\n"

months = [
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec",
]

with open(orbital_file) as f:
    for l in f:
        l = l.strip()

        if "$$SOE" in l:
            is_data = True
            continue

        if is_data == False:
            continue

        if "$$EOE" in l:
            break

        if dataline:
            parsed_l = re.split(r"[XYZ] =", l)
            csv_line = ",".join(
                [x.strip() for x in parsed_l[1:]] + date_val + [filename]
            )
            csv_text += csv_line + "\n"
            dataline = False

        if "A.D." in l:
            date_val = re.split(r"\s+", re.split("A.D.", l)[1].lstrip())[0]
            date_val = date_val.split("-")
            date_val[1] = str(months.index(date_val[1]) + 1)
            dataline = True

print(csv_text)
