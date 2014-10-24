(in-package :iot)

(defstruct (technology (:conc-name tech-)
                       (:constructor make-tech))
  name
  cost
  investment
  (multiplier 3))

(defvar *initial-tech-cost* 75000)

(defvar *technologies*
  (list
   (make-tech :name "Direct Deposit"
              :cost (* *initial-tech-cost* (expt 3 0)) :investment :gas-royalties)
   (make-tech :name "Undivided Interest"
              :cost (* *initial-tech-cost* (expt 3 1)) :investment :oil-royalties)
   (make-tech :name "Pipelines"
              :cost (* *initial-tech-cost* (expt 3 2)) :investment :gas-well)
   (make-tech :name "Pump Jacks"
              :cost (* *initial-tech-cost* (expt 3 3)) :investment :oil-well)
   (make-tech :name "Heavy Excavators"
              :cost (* *initial-tech-cost* (expt 3 4)) :investment :oil-sands)
   (make-tech :name "Fracking"
              :cost (* *initial-tech-cost* (expt 3 5)) :investment :shale-play)
   (make-tech :name "Political Bribes"
              :cost (* *initial-tech-cost* (expt 3 6)) :investment :omani-field)
   (make-tech :name "OPEC Support"
              :cost (* *initial-tech-cost* (expt 3 7)) :investment :saudi-field)
   (make-tech :name "In house Refining"
              :cost (* *initial-tech-cost* (expt 3 8)) :investment :all)
   (make-tech :name "MLP"
              :cost (* *initial-tech-cost* (expt 3 9)) :investment :gas-royalties)
   (make-tech :name "Royalty Trust"
              :cost (* *initial-tech-cost* (expt 3 10)) :investment :oil-royalties)
   (make-tech :name "Natural Gas Liquids"
              :cost (* *initial-tech-cost* (expt 3 11)) :investment :gas-well)
   (make-tech :name "Crude Trains"
              :cost (* *initial-tech-cost* (expt 3 12)) :investment :oil-well)
   (make-tech :name "Petroleum Coke Recycling"
              :cost (* *initial-tech-cost* (expt 3 13)) :investment :oil-sands)
   (make-tech :name "Royalty Sales"
              :cost (* *initial-tech-cost* (expt 3 14)) :investment :shale-play)
   (make-tech :name "LNG Exports"
              :cost (* *initial-tech-cost* (expt 3 15)) :investment :omani-field)
   (make-tech :name "Pipeline to Europe"
              :cost (* *initial-tech-cost* (expt 3 16)) :investment :saudi-field)
   (make-tech :name "Geologists"
              :cost (* *initial-tech-cost* (expt 3 17)) :investment :all)
   (make-tech :name "Tax Loopholes"
              :cost (* *initial-tech-cost* (expt 3 18)) :investment :gas-royalties)
   (make-tech :name "Lobbyists"
              :cost (* *initial-tech-cost* (expt 3 19)) :investment :oil-royalties)
   (make-tech :name "Gathering Plants"
              :cost (* *initial-tech-cost* (expt 3 20)) :investment :gas-well)
   (make-tech :name "Gas Injection"
              :cost (* *initial-tech-cost* (expt 3 21)) :investment :oil-well)
   (make-tech :name "Dredge Lines"
              :cost (* *initial-tech-cost* (expt 3 22)) :investment :oil-sands)
   (make-tech :name "Marcellus Shale"
              :cost (* *initial-tech-cost* (expt 3 23)) :investment :shale-play)
   (make-tech :name "Panamax tankers"
              :cost (* *initial-tech-cost* (expt 3 24)) :investment :omani-field)
   (make-tech :name "EOR Techniques"
              :cost (* *initial-tech-cost* (expt 3 25)) :investment :saudi-field)
   (make-tech :name "Direct Consumer Sales"
              :cost (* *initial-tech-cost* (expt 3 26)) :investment :all)
   (make-tech :name "Online Purchases"
              :cost (* *initial-tech-cost* (expt 3 27)) :investment :gas-royalties)
   (make-tech :name "Speculative Mineral Rights"
              :cost (* *initial-tech-cost* (expt 3 28)) :investment :oil-royalties)
   (make-tech :name "CO2 Recovery"
              :cost (* *initial-tech-cost* (expt 3 29)) :investment :gas-well)
   (make-tech :name "Polymer Flooding"
              :cost (* *initial-tech-cost* (expt 3 30)) :investment :oil-well)
   (make-tech :name "Steam Simulation"
              :cost (* *initial-tech-cost* (expt 3 31)) :investment :oil-sands)
   (make-tech :name "Barnett Shale"
              :cost (* *initial-tech-cost* (expt 3 32)) :investment :shale-play)
   (make-tech :name "Global Delivery Network"
              :cost (* *initial-tech-cost* (expt 3 33)) :investment :omani-field)
   (make-tech :name "Suezmax Tankers"
              :cost (* *initial-tech-cost* (expt 3 34)) :investment :saudi-field)
   (make-tech :name "Plastic Production"
              :cost (* *initial-tech-cost* (expt 3 35)) :investment :all)
   (make-tech :name "Neighborhood Buyouts"
              :cost (* *initial-tech-cost* (expt 3 36)) :investment :gas-royalties)
   (make-tech :name "New Development Buyouts"
              :cost (* *initial-tech-cost* (expt 3 37)) :investment :oil-royalties)
   (make-tech :name "Pipeline Cleaning"
              :cost (* *initial-tech-cost* (expt 3 38)) :investment :gas-well)
   (make-tech :name "Steam Flooding"
              :cost (* *initial-tech-cost* (expt 3 39)) :investment :oil-well)
   (make-tech :name "Light Crude Mixing"
              :cost (* *initial-tech-cost* (expt 3 40)) :investment :oil-sands)
   (make-tech :name "Eagle Ford Shale"
              :cost (* *initial-tech-cost* (expt 3 41)) :investment :shale-play)
   (make-tech :name "VLCC's"
              :cost (* *initial-tech-cost* (expt 3 42)) :investment :omani-field)
   (make-tech :name "ULCC's"
              :cost (* *initial-tech-cost* (expt 3 43)) :investment :saudi-field)
   (make-tech :name "Low Sulfur Diesel"
              :cost (* *initial-tech-cost* (expt 3 44)) :investment :all)
   (make-tech :name "Buyout Farmers"
              :cost (* *initial-tech-cost* (expt 3 45)) :investment :gas-royalties)
   (make-tech :name "South American Royalties"
              :cost (* *initial-tech-cost* (expt 3 46)) :investment :oil-royalties)
   (make-tech :name "Gas Compression"
              :cost (* *initial-tech-cost* (expt 3 47)) :investment :gas-well)
   (make-tech :name "Optimized Crack Spread"
              :cost (* *initial-tech-cost* (expt 3 48)) :investment :oil-well)
   (make-tech :name "Naphtha Recovery"
              :cost (* *initial-tech-cost* (expt 3 49)) :investment :oil-sands)
   (make-tech :name "Bakken Shale"
              :cost (* *initial-tech-cost* (expt 3 50)) :investment :shale-play)
   (make-tech :name "Japanese Exports"
              :cost (* *initial-tech-cost* (expt 3 51)) :investment :omani-field)
   (make-tech :name "Foreign Investment"
              :cost (* *initial-tech-cost* (expt 3 52)) :investment :saudi-field)
   (make-tech :name "Radio Commercials"
              :cost (* *initial-tech-cost* (expt 3 53)) :investment :all)
   (make-tech :name "Middle Eastern Royalties"
              :cost (* *initial-tech-cost* (expt 3 54)) :investment :gas-royalties)
   (make-tech :name "Buyout Ranchers"
              :cost (* *initial-tech-cost* (expt 3 55)) :investment :oil-royalties)
   (make-tech :name "Condensate Sales"
              :cost (* *initial-tech-cost* (expt 3 56)) :investment :gas-well)
   (make-tech :name "Reservoir Modeling"
              :cost (* *initial-tech-cost* (expt 3 57)) :investment :oil-well)
   (make-tech :name "Self Driving Machinery"
              :cost (* *initial-tech-cost* (expt 3 58)) :investment :oil-sands)
   (make-tech :name "Self Directed Drilling Rigs"
              :cost (* *initial-tech-cost* (expt 3 59)) :investment :shale-play)
   (make-tech :name "Political Stability"
              :cost (* *initial-tech-cost* (expt 3 60)) :investment :omani-field)
   (make-tech :name "Chinese Exports"
              :cost (* *initial-tech-cost* (expt 3 61)) :investment :saudi-field)
   (make-tech :name "TV Commercials"
              :cost (* *initial-tech-cost* (expt 3 62)) :investment :all)
   (make-tech :name "Quant Investing"
              :cost (* *initial-tech-cost* (expt 3 63)) :investment :gas-royalties)
   (make-tech :name "High Speed Trading"
              :cost (* *initial-tech-cost* (expt 3 64)) :investment :oil-royalties)
   (make-tech :name "Natural Gas Buses"
              :cost (* *initial-tech-cost* (expt 3 65)) :investment :gas-well)
   (make-tech :name "Offshore Wells"
              :cost (* *initial-tech-cost* (expt 3 66)) :investment :oil-well)
   (make-tech :name "Coal Bed Methane"
              :cost (* *initial-tech-cost* (expt 3 67)) :investment :oil-sands)
   (make-tech :name "Permian Basin"
              :cost (* *initial-tech-cost* (expt 3 68)) :investment :shale-play)
   (make-tech :name "Worldwide Pipelines"
              :cost (* *initial-tech-cost* (expt 3 69)) :investment :omani-field)
   (make-tech :name "Tertiary Recovery"
              :cost (* *initial-tech-cost* (expt 3 70)) :investment :saudi-field)
   (make-tech :name "Home Gas Pumps"
              :cost (* *initial-tech-cost* (expt 3 71)) :investment :all)
   (make-tech :name "Exchange Traded Funds"
              :cost (* *initial-tech-cost* (expt 3 72)) :investment :gas-royalties)
   (make-tech :name "Technical Analysis"
              :cost (* *initial-tech-cost* (expt 3 73)) :investment :oil-royalties)
   (make-tech :name "Natural Gas Cars"
              :cost (* *initial-tech-cost* (expt 3 74)) :investment :gas-well)
   (make-tech :name "Oil Burning Power Plant"
              :cost (* *initial-tech-cost* (expt 3 75)) :investment :oil-well)
   (make-tech :name "In-Situ Recovery"
              :cost (* *initial-tech-cost* (expt 3 76)) :investment :oil-sands)
   (make-tech :name "Horizontal Drilling"
              :cost (* *initial-tech-cost* (expt 3 77)) :investment :shale-play)
   (make-tech :name "Exclusive Contracts"
              :cost (* *initial-tech-cost* (expt 3 78)) :investment :omani-field)
   (make-tech :name "Third World Development"
              :cost (* *initial-tech-cost* (expt 3 79)) :investment :saudi-field)
   (make-tech :name "Synthetic Oil"
              :cost (* *initial-tech-cost* (expt 3 80)) :investment :all)
   (make-tech :name "Fee Simple Ownership"
              :cost (* *initial-tech-cost* (expt 3 81)) :investment :gas-royalties)
   (make-tech :name "Federal Frontier Lands"
              :cost (* *initial-tech-cost* (expt 3 82)) :investment :oil-royalties)
   (make-tech :name "Hydrogen Production"
              :cost (* *initial-tech-cost* (expt 3 83)) :investment :gas-well)
   (make-tech :name "Multiple Well Pads"
              :cost (* *initial-tech-cost* (expt 3 84)) :investment :oil-well)
   (make-tech :name "Toe to Heel Air Injection"
              :cost (* *initial-tech-cost* (expt 3 85)) :investment :oil-sands)
   (make-tech :name "Super Frack"
              :cost (* *initial-tech-cost* (expt 3 86)) :investment :shale-play)
   (make-tech :name "Field Modernization"
              :cost (* *initial-tech-cost* (expt 3 87)) :investment :omani-field)
   (make-tech :name "Pipeline Expansions"
              :cost (* *initial-tech-cost* (expt 3 88)) :investment :saudi-field)
   (make-tech :name "First One Free"
              :cost (* *initial-tech-cost* (expt 3 89)) :investment :all)
   (make-tech :name "36% Royalties"
              :cost (* *initial-tech-cost* (expt 3 90)) :investment :gas-royalties)
   (make-tech :name "42% Royalties"
              :cost (* *initial-tech-cost* (expt 3 91)) :investment :oil-royalties)
   (make-tech :name "Graphene Production"
              :cost (* *initial-tech-cost* (expt 3 92)) :investment :gas-well)
   (make-tech :name "Full Reservoir Recovery"
              :cost (* *initial-tech-cost* (expt 3 93)) :investment :oil-well)
   (make-tech :name "Overhead Gravity Drainage"
              :cost (* *initial-tech-cost* (expt 3 94)) :investment :oil-sands)
   (make-tech :name "Mega Frack"
              :cost (* *initial-tech-cost* (expt 3 95)) :investment :shale-play)
   (make-tech :name "Proved Reserves Doubled"
              :cost (* *initial-tech-cost* (expt 3 96)) :investment :omani-field)
   (make-tech :name "New Fields Developed"
              :cost (* *initial-tech-cost* (expt 3 97)) :investment :saudi-field)
   (make-tech :name "Second One Costs Double"
              :cost (* *initial-tech-cost* (expt 3 98)) :investment :all)
   (make-tech :name "All The Royalties"
              :cost (* *initial-tech-cost* (expt 3 99)) :investment :gas-royalties)
   (make-tech :name "And Then More"
              :cost (* *initial-tech-cost* (expt 3 100)) :investment :oil-royalties)
   (make-tech :name "Buy Russia"
              :cost (* *initial-tech-cost* (expt 3 101)) :investment :gas-well)
   (make-tech :name "Buy Alaska"
              :cost (* *initial-tech-cost* (expt 3 102)) :investment :oil-well)
   (make-tech :name "Buy Canada"
              :cost (* *initial-tech-cost* (expt 3 103)) :investment :oil-sands)
   (make-tech :name "Buy Texas"
              :cost (* *initial-tech-cost* (expt 3 104)) :investment :shale-play)
   (make-tech :name "Buy Oman"
              :cost (* *initial-tech-cost* (expt 3 105)) :investment :omani-field)
   (make-tech :name "Buy Saudi Arabia"
              :cost (* *initial-tech-cost* (expt 3 106)) :investment :saudi-field)
   (make-tech :name "Buy Earth"
              :cost (* *initial-tech-cost* (expt 3 107)) :investment :all)
   (make-tech :name "Carbon Offsets"
              :cost (* *initial-tech-cost* (expt 3 108)) :investment :gas-royalties :multiplier 10)
   (make-tech :name "Green Image"
              :cost (* *initial-tech-cost* (expt 3 109)) :investment :oil-royalties :multiplier 9)
   (make-tech :name "Environmentalist Support"
              :cost (* *initial-tech-cost* (expt 3 110)) :investment :gas-well      :multiplier 8)
   (make-tech :name "Earth Day Sponsor"
              :cost (* *initial-tech-cost* (expt 3 111)) :investment :oil-well      :multiplier 7)
   (make-tech :name "Earth Week Sponsor"
              :cost (* *initial-tech-cost* (expt 3 112)) :investment :oil-sands     :multiplier 6)
   (make-tech :name "Earth Month Sponsor"
              :cost (* *initial-tech-cost* (expt 3 113)) :investment :shale-play    :multiplier 5)
   (make-tech :name "Earth Year Sponsor"
              :cost (* *initial-tech-cost* (expt 3 114)) :investment :omani-field   :multiplier 4)
   (make-tech :name "Earth Decade Sponsor"
              :cost (* *initial-tech-cost* (expt 3 115)) :investment :saudi-field   :multiplier 3)
   (make-tech :name "Carbon Sequestration"
              :cost (* *initial-tech-cost* (expt 3 116)) :investment :all           :multiplier 3)
   (make-tech :name "Publicist"
              :cost (* *initial-tech-cost* (expt 3 117)) :investment :gas-royalties)
   (make-tech :name "PR Firm"
              :cost (* *initial-tech-cost* (expt 3 118)) :investment :oil-royalties)
   (make-tech :name "Good Publicity"
              :cost (* *initial-tech-cost* (expt 3 119)) :investment :gas-well)
   (make-tech :name "Better Publicity"
              :cost (* *initial-tech-cost* (expt 3 120)) :investment :oil-well)
   (make-tech :name "Great Publicity"
              :cost (* *initial-tech-cost* (expt 3 121)) :investment :oil-sands)
   (make-tech :name "Superb Publicity"
              :cost (* *initial-tech-cost* (expt 3 122)) :investment :shale-play)
   (make-tech :name "Stellar Publicity"
              :cost (* *initial-tech-cost* (expt 3 123)) :investment :omani-field)
   (make-tech :name "Incredible Publicity"
              :cost (* *initial-tech-cost* (expt 3 124)) :investment :saudi-field)
   (make-tech :name "Magnificent Publicity"
              :cost (* *initial-tech-cost* (expt 3 125)) :investment :all)
   (make-tech :name "Producer Oversight"
              :cost (* *initial-tech-cost* (expt 3 126)) :investment :gas-royalties)
   (make-tech :name "Income Diversity"
              :cost (* *initial-tech-cost* (expt 3 127)) :investment :oil-royalties)
   (make-tech :name "New Extraction Techniques"
              :cost (* *initial-tech-cost* (expt 3 128)) :investment :gas-well)
   (make-tech :name "Super Tight Oil Recovery"
              :cost (* *initial-tech-cost* (expt 3 129)) :investment :oil-well)
   (make-tech :name "Bitumen Powered Cars"
              :cost (* *initial-tech-cost* (expt 3 130)) :investment :oil-sands)
   (make-tech :name "Economic Recovery"
              :cost (* *initial-tech-cost* (expt 3 131)) :investment :shale-play)
   (make-tech :name "Deep Oil Fields"
              :cost (* *initial-tech-cost* (expt 3 132)) :investment :omani-field)
   (make-tech :name "Sand to Oil"
              :cost (* *initial-tech-cost* (expt 3 133)) :investment :saudi-field)
   (make-tech :name "Instant Oil Extraction"
              :cost (* *initial-tech-cost* (expt 3 134)) :investment :all)
   (make-tech :name "Extraterrestrial Gas Royalties"
              :cost (* *initial-tech-cost* (expt 3 135)) :investment :gas-royalties :multiplier 10)
   (make-tech :name "Extraterrestrial Oil Royalties"
              :cost (* *initial-tech-cost* (expt 3 136)) :investment :oil-royalties :multiplier 9)
   (make-tech :name "Extraterrestrial Gas Wells"
              :cost (* *initial-tech-cost* (expt 3 137)) :investment :gas-well      :multiplier 8)
   (make-tech :name "Extraterrestrial Oil Wells"
              :cost (* *initial-tech-cost* (expt 3 138)) :investment :oil-well      :multiplier 7)
   (make-tech :name "Extraterrestrial Oil Sands"
              :cost (* *initial-tech-cost* (expt 3 139)) :investment :oil-sands     :multiplier 6)
   (make-tech :name "Extraterrestrial Shale Plays"
              :cost (* *initial-tech-cost* (expt 3 140)) :investment :shale-play    :multiplier 5)
   (make-tech :name "Oman Expansion"
              :cost (* *initial-tech-cost* (expt 3 141)) :investment :omani-field   :multiplier 4)
   (make-tech :name "Saudi Expansion"
              :cost (* *initial-tech-cost* (expt 3 142)) :investment :saudi-field   :multiplier 3)
   (make-tech :name "Galactic Oil Business"
              :cost (* *initial-tech-cost* (expt 3 143)) :investment :all           :multiplier 5)
   (make-tech :name "Universal Oil Business"
              :cost (expt 10 80)                         :investment :all           :multiplier 5)))

(defun tech-mult (techs investment-key)
  (iterate (for tech in techs)
           (when (or (eql (tech-investment tech) investment-key)
                     (eql (tech-investment tech) :all))
             (multiplying (tech-multiplier tech)))))

(defun techs-before (name)
  (iterate (for technology in *technologies*)
           (when (equal name (tech-name technology))
             (finish))
           (collecting technology)))
