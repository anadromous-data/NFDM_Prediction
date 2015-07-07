load 'final_cheese.rb'

# LOAD THE CURRENT DATA LIST #
# d2g equals for 'data to get' 

d2g = CSV.read('data_list.csv', :headers => true)

# RUN DATA SCRAPER ADD TO INCOMING #

until d2g.empty?
  puts "Running #{d2g[0][0]}"
  YahooFinanceData.run(d2g[0][0], d2g[0][3], d2g[0][4], d2g[0][5]) 
  d2g.delete(0)
  # manually fetch production data from http://usda.mannlib.cornell.edu/MannUsda/viewDocumentInfo.do?documentID=1052
  # manually fetch slaughter data from http://usda.mannlib.cornell.edu/MannUsda/viewDocumentInfo.do?documentID=1096
end

# i_fin equals incoming financial data
i_fin = Dir['incoming/fin/*.csv']

# RUN CLEANER TO FORMAT CSVs #
i_fin.each do |i|
  CleanFinancialData.run(i)
end