from db_module import *
import pymssql

#Change this according to the your database information and your network model date and version
con = pymssql.connect(server='thales-mathsys1.database.windows.net', user='ayman-admin@thales-mathsys1', password='xxxxxxxxxx', database='thales')
date = "2016-03-29"
version = "4.1"

with con:
    insert_nodes(date, version, con)
    print "Nodes Done!"
    insert_links(date, version, con)
    print "Links Done!"
    insert_sites(date, version, con)
    print "Sites Done!"
    insert_measurements(date, version, con)
    print "Measurements Done!"
