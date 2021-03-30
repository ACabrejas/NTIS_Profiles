def get_m25_sites(con):
    cur = con.cursor()
    cur.execute("SELECT site_id FROM sites WHERE ((longitude >  -0.55 AND longitude < -0.35) AND (latitude > 51.30 AND latitude < 51.75) AND (site_reference LIKE 'M25%'))")
    needed_sites = [site[0] for site in cur.fetchall()]
    return needed_sites

def get_m11_sites(con):
    cur = con.cursor()
    cur.execute("SELECT site_id FROM sites WHERE (site_reference LIKE 'M11%')")
    needed_sites = [site[0] for site in cur.fetchall()]
    return needed_sites

def get_m6_sites(con):
    cur = con.cursor()
    cur.execute("SELECT site_id FROM sites WHERE ((longitude > -2.707 AND longitude < -2.625) AND (latitude > 53.709 AND latitude < 53.810) AND (site_reference LIKE 'M6%'))")
    needed_sites = [site[0] for site in cur.fetchall()]
    return needed_sites

def get_selected_sites(con):
    cur = con.cursor()
    links_list = "(125039001, 199055902, 200024946, 199063301, 199041001, 199048801, 199063701, 200023893, 199050901, 199065202, 199043302, 199063303, 199041501, 199049101, 117008401, 117016001, 117007801, 117007703, 117012201, 117012301, 117007501, 199064902)"
    query = "SELECT site_id FROM sites WHERE link_id IN {}".format(links_list)
    cur.execute(query)
    needed_sites = [site[0] for site in cur.fetchall()]
    return needed_sites
