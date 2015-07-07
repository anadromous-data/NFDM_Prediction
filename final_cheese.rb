#            ^__^
#  moooo     (oo)_______
#            (__) o  0  )\/*
#              /||----w |\
# || ||\| //| //||     || \
############################
# CHEESE #
##########

# 1. Get Data
        
#          Currently it is not an efficient use of time 
#          to try and untangle the logic over a large number 
#          of data sets to produce a monthly update for our model.
#          Instructions will be provided. 

#  2. Format Data
        
#         Once data has been collected and placed in the appropriate 
#         folders, it will be consolidated and ready to be pushed to 
#         the master template.

#  3. Add data to master template
        
#         Data in the staging template is updated to an updated master
#         template. 


require 'csv'
require 'time'
require 'open-uri'
require 'net/http'

# DATA GETTER CLASSES #
class GetData
  def self.run(save_as, url)
    open("/Users/stuartillson/python_projects/final_cheese/incoming/#{save_as}", 'wb') do |file|
      file << open("#{url}").read
    end
  end
end

class YahooFinanceData < GetData
  
  def self.add_leading_zero(string_digit)
    if string_digit.length == 1
      string_digit = "0" + string_digit
    else
      string_digit = string_digit
    end
  end

  def self.run(save_as, code, date1, date2)
    start_date = Date.parse(date1)
    end_date = Date.parse(date2)
    start_month = YahooFinanceData.add_leading_zero(((start_date.month - 1).to_s))    
    start_day = start_date.day   
    start_year = start_date.year
    end_month = YahooFinanceData.add_leading_zero(((end_date.month - 1).to_s))
    end_day = end_date.day
    end_year = end_date.year

    uri = URI("http://ichart.finance.yahoo.com/table.csv?s=#{(code)}&a=#{start_month}&b=#{start_day}&c=#{start_year}&d=#{end_month}&e=#{end_day}&f=#{end_year}&g=d&ignore=.csv")
    conn = Net::HTTP.get(uri)
    result = CSV.parse(conn, :headers => true)
    
    CSV.open("/Users/stuartillson/python_projects/final_cheese/incoming/fin/#{save_as}.csv", 'wb') do |csv|
      csv << result.headers
      until result.empty?
        csv << result[0]
        result.delete(0)
      end
    end
  end
end

class ProductionData < GetData
  # Code to come if relevant...
end


# CLEAN AND ADD TO OUTGOING #

class CleanFinancialData
  def self.run(csv_file)
    csv = CSV.read(csv_file).reverse
    csv.pop

    name = csv_file.split('/').last.split('.').first
    low_date = Date.parse(csv[0][0])
    end_date = Date.parse(csv.last[0])

    CSV.open("outgoing/fin/#{name}.csv", 'wb') do |write|
      write << ["Date"]
      while low_date < end_date
        if Date.parse(csv[0][0]) == low_date
          row = csv[0]
          write << ["#{low_date.to_s}", "#{row[1]}", "#{row[2]}", "#{row[3]}", "#{row[4]}", "#{row[5]}", "#{row[6]}"]
          csv.shift
        else
          write << ["#{low_date.to_s}"]
        end

        low_date = (low_date + 1)
      end
    end
  end
end


# ## CLOSE SOURCE BSD-2 CLAUSE ##
# Copyright (c) <2014>, <Stuart James Illson>
# All rights reserved.

# Redistribution and use in source and binary forms, with or without modification, 
# are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice, 
# this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright notice, 
# this list of conditions and the following disclaimer in the documentation and/or 
# other materials provided with the distribution.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY 
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT 
# SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT 
# OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

