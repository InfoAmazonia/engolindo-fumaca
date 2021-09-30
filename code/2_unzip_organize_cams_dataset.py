import zipfile
import glob
import os
wd = '/media/felipe/DATA/Repos/EngolindoFumaca/Data/Raw/CAMS/CDS/'
zip_files = glob.glob(f'{wd}*.zip')

for zip_file in zip_files:
    # zip_file = zip_files[0]
    zipdata = zipfile.ZipFile(zip_file)
    zipinfos = zipdata.infolist()
    # iterate through each file
    for zipinfo in zipinfos:
        # zipinfo = zipinfos[0]
        # This will do the renaming
        zipinfo.filename = os.path.basename(zip_file).replace('.zip', '.nc')
        zipdata.extract(zipinfo, wd)
