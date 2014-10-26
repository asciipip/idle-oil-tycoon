(in-package :iot)

(defstruct (technology (:conc-name tech-)
                       (:constructor make-tech))
  name
  cost
  property
  (multiplier 3))

(defvar *initial-tech-cost* 75000)

(defvar *technologies*
  (list
   (make-tech :name "Direct Deposit"
              :cost (* *initial-tech-cost* (expt 3 0)) :property :gas-royalties)
   (make-tech :name "Undivided Interest"
              :cost (* *initial-tech-cost* (expt 3 1)) :property :oil-royalties)
   (make-tech :name "Pipelines"
              :cost (* *initial-tech-cost* (expt 3 2)) :property :gas-well)
   (make-tech :name "Pump Jacks"
              :cost (* *initial-tech-cost* (expt 3 3)) :property :oil-well)
   (make-tech :name "Heavy Excavators"
              :cost (* *initial-tech-cost* (expt 3 4)) :property :oil-sands)
   (make-tech :name "Fracking"
              :cost (* *initial-tech-cost* (expt 3 5)) :property :shale-play)
   (make-tech :name "Political Bribes"
              :cost (* *initial-tech-cost* (expt 3 6)) :property :omani-field)
   (make-tech :name "OPEC Support"
              :cost (* *initial-tech-cost* (expt 3 7)) :property :saudi-field)
   (make-tech :name "In house Refining"
              :cost (* *initial-tech-cost* (expt 3 8)) :property :all)
   (make-tech :name "MLP"
              :cost (* *initial-tech-cost* (expt 3 9)) :property :gas-royalties)
   (make-tech :name "Royalty Trust"
              :cost (* *initial-tech-cost* (expt 3 10)) :property :oil-royalties)
   (make-tech :name "Natural Gas Liquids"
              :cost (* *initial-tech-cost* (expt 3 11)) :property :gas-well)
   (make-tech :name "Crude Trains"
              :cost (* *initial-tech-cost* (expt 3 12)) :property :oil-well)
   (make-tech :name "Petroleum Coke Recycling"
              :cost (* *initial-tech-cost* (expt 3 13)) :property :oil-sands)
   (make-tech :name "Royalty Sales"
              :cost (* *initial-tech-cost* (expt 3 14)) :property :shale-play)
   (make-tech :name "LNG Exports"
              :cost (* *initial-tech-cost* (expt 3 15)) :property :omani-field)
   (make-tech :name "Pipeline to Europe"
              :cost (* *initial-tech-cost* (expt 3 16)) :property :saudi-field)
   (make-tech :name "Geologists"
              :cost (* *initial-tech-cost* (expt 3 17)) :property :all)
   (make-tech :name "Tax Loopholes"
              :cost (* *initial-tech-cost* (expt 3 18)) :property :gas-royalties)
   (make-tech :name "Lobbyists"
              :cost (* *initial-tech-cost* (expt 3 19)) :property :oil-royalties)
   (make-tech :name "Gathering Plants"
              :cost (* *initial-tech-cost* (expt 3 20)) :property :gas-well)
   (make-tech :name "Gas Injection"
              :cost (* *initial-tech-cost* (expt 3 21)) :property :oil-well)
   (make-tech :name "Dredge Lines"
              :cost (* *initial-tech-cost* (expt 3 22)) :property :oil-sands)
   (make-tech :name "Marcellus Shale"
              :cost (* *initial-tech-cost* (expt 3 23)) :property :shale-play)
   (make-tech :name "Panamax tankers"
              :cost (* *initial-tech-cost* (expt 3 24)) :property :omani-field)
   (make-tech :name "EOR Techniques"
              :cost (* *initial-tech-cost* (expt 3 25)) :property :saudi-field)
   (make-tech :name "Direct Consumer Sales"
              :cost (* *initial-tech-cost* (expt 3 26)) :property :all)
   (make-tech :name "Online Purchases"
              :cost (* *initial-tech-cost* (expt 3 27)) :property :gas-royalties)
   (make-tech :name "Speculative Mineral Rights"
              :cost (* *initial-tech-cost* (expt 3 28)) :property :oil-royalties)
   (make-tech :name "CO2 Recovery"
              :cost (* *initial-tech-cost* (expt 3 29)) :property :gas-well)
   (make-tech :name "Polymer Flooding"
              :cost (* *initial-tech-cost* (expt 3 30)) :property :oil-well)
   (make-tech :name "Steam Simulation"
              :cost (* *initial-tech-cost* (expt 3 31)) :property :oil-sands)
   (make-tech :name "Barnett Shale"
              :cost (* *initial-tech-cost* (expt 3 32)) :property :shale-play)
   (make-tech :name "Global Delivery Network"
              :cost (* *initial-tech-cost* (expt 3 33)) :property :omani-field)
   (make-tech :name "Suezmax Tankers"
              :cost (* *initial-tech-cost* (expt 3 34)) :property :saudi-field)
   (make-tech :name "Plastic Production"
              :cost (* *initial-tech-cost* (expt 3 35)) :property :all)
   (make-tech :name "Neighborhood Buyouts"
              :cost (* *initial-tech-cost* (expt 3 36)) :property :gas-royalties)
   (make-tech :name "New Development Buyouts"
              :cost (* *initial-tech-cost* (expt 3 37)) :property :oil-royalties)
   (make-tech :name "Pipeline Cleaning"
              :cost (* *initial-tech-cost* (expt 3 38)) :property :gas-well)
   (make-tech :name "Steam Flooding"
              :cost (* *initial-tech-cost* (expt 3 39)) :property :oil-well)
   (make-tech :name "Light Crude Mixing"
              :cost (* *initial-tech-cost* (expt 3 40)) :property :oil-sands)
   (make-tech :name "Eagle Ford Shale"
              :cost (* *initial-tech-cost* (expt 3 41)) :property :shale-play)
   (make-tech :name "VLCC's"
              :cost (* *initial-tech-cost* (expt 3 42)) :property :omani-field)
   (make-tech :name "ULCC's"
              :cost (* *initial-tech-cost* (expt 3 43)) :property :saudi-field)
   (make-tech :name "Low Sulfur Diesel"
              :cost (* *initial-tech-cost* (expt 3 44)) :property :all)
   (make-tech :name "Buyout Farmers"
              :cost (* *initial-tech-cost* (expt 3 45)) :property :gas-royalties)
   (make-tech :name "South American Royalties"
              :cost (* *initial-tech-cost* (expt 3 46)) :property :oil-royalties)
   (make-tech :name "Gas Compression"
              :cost (* *initial-tech-cost* (expt 3 47)) :property :gas-well)
   (make-tech :name "Optimized Crack Spread"
              :cost (* *initial-tech-cost* (expt 3 48)) :property :oil-well)
   (make-tech :name "Naphtha Recovery"
              :cost (* *initial-tech-cost* (expt 3 49)) :property :oil-sands)
   (make-tech :name "Bakken Shale"
              :cost (* *initial-tech-cost* (expt 3 50)) :property :shale-play)
   (make-tech :name "Japanese Exports"
              :cost (* *initial-tech-cost* (expt 3 51)) :property :omani-field)
   (make-tech :name "Foreign Investment"
              :cost (* *initial-tech-cost* (expt 3 52)) :property :saudi-field)
   (make-tech :name "Radio Commercials"
              :cost (* *initial-tech-cost* (expt 3 53)) :property :all)
   (make-tech :name "Middle Eastern Royalties"
              :cost (* *initial-tech-cost* (expt 3 54)) :property :gas-royalties)
   (make-tech :name "Buyout Ranchers"
              :cost (* *initial-tech-cost* (expt 3 55)) :property :oil-royalties)
   (make-tech :name "Condensate Sales"
              :cost (* *initial-tech-cost* (expt 3 56)) :property :gas-well)
   (make-tech :name "Reservoir Modeling"
              :cost (* *initial-tech-cost* (expt 3 57)) :property :oil-well)
   (make-tech :name "Self Driving Machinery"
              :cost (* *initial-tech-cost* (expt 3 58)) :property :oil-sands)
   (make-tech :name "Self Directed Drilling Rigs"
              :cost (* *initial-tech-cost* (expt 3 59)) :property :shale-play)
   (make-tech :name "Political Stability"
              :cost (* *initial-tech-cost* (expt 3 60)) :property :omani-field)
   (make-tech :name "Chinese Exports"
              :cost (* *initial-tech-cost* (expt 3 61)) :property :saudi-field)
   (make-tech :name "TV Commercials"
              :cost (* *initial-tech-cost* (expt 3 62)) :property :all)
   (make-tech :name "Quant Investing"
              :cost (* *initial-tech-cost* (expt 3 63)) :property :gas-royalties)
   (make-tech :name "High Speed Trading"
              :cost (* *initial-tech-cost* (expt 3 64)) :property :oil-royalties)
   (make-tech :name "Natural Gas Buses"
              :cost (* *initial-tech-cost* (expt 3 65)) :property :gas-well)
   (make-tech :name "Offshore Wells"
              :cost (* *initial-tech-cost* (expt 3 66)) :property :oil-well)
   (make-tech :name "Coal Bed Methane"
              :cost (* *initial-tech-cost* (expt 3 67)) :property :oil-sands)
   (make-tech :name "Permian Basin"
              :cost (* *initial-tech-cost* (expt 3 68)) :property :shale-play)
   (make-tech :name "Worldwide Pipelines"
              :cost (* *initial-tech-cost* (expt 3 69)) :property :omani-field)
   (make-tech :name "Tertiary Recovery"
              :cost (* *initial-tech-cost* (expt 3 70)) :property :saudi-field)
   (make-tech :name "Home Gas Pumps"
              :cost (* *initial-tech-cost* (expt 3 71)) :property :all)
   (make-tech :name "Exchange Traded Funds"
              :cost (* *initial-tech-cost* (expt 3 72)) :property :gas-royalties)
   (make-tech :name "Technical Analysis"
              :cost (* *initial-tech-cost* (expt 3 73)) :property :oil-royalties)
   (make-tech :name "Natural Gas Cars"
              :cost (* *initial-tech-cost* (expt 3 74)) :property :gas-well)
   (make-tech :name "Oil Burning Power Plant"
              :cost (* *initial-tech-cost* (expt 3 75)) :property :oil-well)
   (make-tech :name "In-Situ Recovery"
              :cost (* *initial-tech-cost* (expt 3 76)) :property :oil-sands)
   (make-tech :name "Horizontal Drilling"
              :cost (* *initial-tech-cost* (expt 3 77)) :property :shale-play)
   (make-tech :name "Exclusive Contracts"
              :cost (* *initial-tech-cost* (expt 3 78)) :property :omani-field)
   (make-tech :name "Third World Development"
              :cost (* *initial-tech-cost* (expt 3 79)) :property :saudi-field)
   (make-tech :name "Synthetic Oil"
              :cost (* *initial-tech-cost* (expt 3 80)) :property :all)
   (make-tech :name "Fee Simple Ownership"
              :cost (* *initial-tech-cost* (expt 3 81)) :property :gas-royalties)
   (make-tech :name "Federal Frontier Lands"
              :cost (* *initial-tech-cost* (expt 3 82)) :property :oil-royalties)
   (make-tech :name "Hydrogen Production"
              :cost (* *initial-tech-cost* (expt 3 83)) :property :gas-well)
   (make-tech :name "Multiple Well Pads"
              :cost (* *initial-tech-cost* (expt 3 84)) :property :oil-well)
   (make-tech :name "Toe to Heel Air Injection"
              :cost (* *initial-tech-cost* (expt 3 85)) :property :oil-sands)
   (make-tech :name "Super Frack"
              :cost (* *initial-tech-cost* (expt 3 86)) :property :shale-play)
   (make-tech :name "Field Modernization"
              :cost (* *initial-tech-cost* (expt 3 87)) :property :omani-field)
   (make-tech :name "Pipeline Expansions"
              :cost (* *initial-tech-cost* (expt 3 88)) :property :saudi-field)
   (make-tech :name "First One Free"
              :cost (* *initial-tech-cost* (expt 3 89)) :property :all)
   (make-tech :name "36% Royalties"
              :cost (* *initial-tech-cost* (expt 3 90)) :property :gas-royalties)
   (make-tech :name "42% Royalties"
              :cost (* *initial-tech-cost* (expt 3 91)) :property :oil-royalties)
   (make-tech :name "Graphene Production"
              :cost (* *initial-tech-cost* (expt 3 92)) :property :gas-well)
   (make-tech :name "Full Reservoir Recovery"
              :cost (* *initial-tech-cost* (expt 3 93)) :property :oil-well)
   (make-tech :name "Overhead Gravity Drainage"
              :cost (* *initial-tech-cost* (expt 3 94)) :property :oil-sands)
   (make-tech :name "Mega Frack"
              :cost (* *initial-tech-cost* (expt 3 95)) :property :shale-play)
   (make-tech :name "Proved Reserves Doubled"
              :cost (* *initial-tech-cost* (expt 3 96)) :property :omani-field)
   (make-tech :name "New Fields Developed"
              :cost (* *initial-tech-cost* (expt 3 97)) :property :saudi-field)
   (make-tech :name "Second One Costs Double"
              :cost (* *initial-tech-cost* (expt 3 98)) :property :all)
   (make-tech :name "All The Royalties"
              :cost (* *initial-tech-cost* (expt 3 99)) :property :gas-royalties)
   (make-tech :name "And Then More"
              :cost (* *initial-tech-cost* (expt 3 100)) :property :oil-royalties)
   (make-tech :name "Buy Russia"
              :cost (* *initial-tech-cost* (expt 3 101)) :property :gas-well)
   (make-tech :name "Buy Alaska"
              :cost (* *initial-tech-cost* (expt 3 102)) :property :oil-well)
   (make-tech :name "Buy Canada"
              :cost (* *initial-tech-cost* (expt 3 103)) :property :oil-sands)
   (make-tech :name "Buy Texas"
              :cost (* *initial-tech-cost* (expt 3 104)) :property :shale-play)
   (make-tech :name "Buy Oman"
              :cost (* *initial-tech-cost* (expt 3 105)) :property :omani-field)
   (make-tech :name "Buy Saudi Arabia"
              :cost (* *initial-tech-cost* (expt 3 106)) :property :saudi-field)
   (make-tech :name "Buy Earth"
              :cost (* *initial-tech-cost* (expt 3 107)) :property :all)
   (make-tech :name "Carbon Offsets"
              :cost (* *initial-tech-cost* (expt 3 108)) :property :gas-royalties :multiplier 10)
   (make-tech :name "Green Image"
              :cost (* *initial-tech-cost* (expt 3 109)) :property :oil-royalties :multiplier 9)
   (make-tech :name "Environmentalist Support"
              :cost (* *initial-tech-cost* (expt 3 110)) :property :gas-well      :multiplier 8)
   (make-tech :name "Earth Day Sponsor"
              :cost (* *initial-tech-cost* (expt 3 111)) :property :oil-well      :multiplier 7)
   (make-tech :name "Earth Week Sponsor"
              :cost (* *initial-tech-cost* (expt 3 112)) :property :oil-sands     :multiplier 6)
   (make-tech :name "Earth Month Sponsor"
              :cost (* *initial-tech-cost* (expt 3 113)) :property :shale-play    :multiplier 5)
   (make-tech :name "Earth Year Sponsor"
              :cost (* *initial-tech-cost* (expt 3 114)) :property :omani-field   :multiplier 4)
   (make-tech :name "Earth Decade Sponsor"
              :cost (* *initial-tech-cost* (expt 3 115)) :property :saudi-field   :multiplier 3)
   (make-tech :name "Carbon Sequestration"
              :cost (* *initial-tech-cost* (expt 3 116)) :property :all           :multiplier 3)
   (make-tech :name "Publicist"
              :cost (* *initial-tech-cost* (expt 3 117)) :property :gas-royalties)
   (make-tech :name "PR Firm"
              :cost (* *initial-tech-cost* (expt 3 118)) :property :oil-royalties)
   (make-tech :name "Good Publicity"
              :cost (* *initial-tech-cost* (expt 3 119)) :property :gas-well)
   (make-tech :name "Better Publicity"
              :cost (* *initial-tech-cost* (expt 3 120)) :property :oil-well)
   (make-tech :name "Great Publicity"
              :cost (* *initial-tech-cost* (expt 3 121)) :property :oil-sands)
   (make-tech :name "Superb Publicity"
              :cost (* *initial-tech-cost* (expt 3 122)) :property :shale-play)
   (make-tech :name "Stellar Publicity"
              :cost (* *initial-tech-cost* (expt 3 123)) :property :omani-field)
   (make-tech :name "Incredible Publicity"
              :cost (* *initial-tech-cost* (expt 3 124)) :property :saudi-field)
   (make-tech :name "Magnificent Publicity"
              :cost (* *initial-tech-cost* (expt 3 125)) :property :all)
   (make-tech :name "Producer Oversight"
              :cost (* *initial-tech-cost* (expt 3 126)) :property :gas-royalties)
   (make-tech :name "Income Diversity"
              :cost (* *initial-tech-cost* (expt 3 127)) :property :oil-royalties)
   (make-tech :name "New Extraction Techniques"
              :cost (* *initial-tech-cost* (expt 3 128)) :property :gas-well)
   (make-tech :name "Super Tight Oil Recovery"
              :cost (* *initial-tech-cost* (expt 3 129)) :property :oil-well)
   (make-tech :name "Bitumen Powered Cars"
              :cost (* *initial-tech-cost* (expt 3 130)) :property :oil-sands)
   (make-tech :name "Economic Recovery"
              :cost (* *initial-tech-cost* (expt 3 131)) :property :shale-play)
   (make-tech :name "Deep Oil Fields"
              :cost (* *initial-tech-cost* (expt 3 132)) :property :omani-field)
   (make-tech :name "Sand to Oil"
              :cost (* *initial-tech-cost* (expt 3 133)) :property :saudi-field)
   (make-tech :name "Instant Oil Extraction"
              :cost (* *initial-tech-cost* (expt 3 134)) :property :all)
   (make-tech :name "Extraterrestrial Gas Royalties"
              :cost (* *initial-tech-cost* (expt 3 135)) :property :gas-royalties :multiplier 10)
   (make-tech :name "Extraterrestrial Oil Royalties"
              :cost (* *initial-tech-cost* (expt 3 136)) :property :oil-royalties :multiplier 9)
   (make-tech :name "Extraterrestrial Gas Wells"
              :cost (* *initial-tech-cost* (expt 3 137)) :property :gas-well      :multiplier 8)
   (make-tech :name "Extraterrestrial Oil Wells"
              :cost (* *initial-tech-cost* (expt 3 138)) :property :oil-well      :multiplier 7)
   (make-tech :name "Extraterrestrial Oil Sands"
              :cost (* *initial-tech-cost* (expt 3 139)) :property :oil-sands     :multiplier 6)
   (make-tech :name "Extraterrestrial Shale Plays"
              :cost (* *initial-tech-cost* (expt 3 140)) :property :shale-play    :multiplier 5)
   (make-tech :name "Oman Expansion"
              :cost (* *initial-tech-cost* (expt 3 141)) :property :omani-field   :multiplier 4)
   (make-tech :name "Saudi Expansion"
              :cost (* *initial-tech-cost* (expt 3 142)) :property :saudi-field   :multiplier 3)
   (make-tech :name "Galactic Oil Business"
              :cost (* *initial-tech-cost* (expt 3 143)) :property :all           :multiplier 5)
   (make-tech :name "Universal Oil Business"
              :cost (expt 10 80)                         :property :all           :multiplier 5)))

(defun tech-mult (techs property-or-key)
  (iterate (for tech in techs)
           (when (or (eql (tech-property tech) (prop-key property-or-key))
                     (eql (tech-property tech) :all))
             (multiplying (tech-multiplier tech)))))

(defun techs-before (name)
  (iterate (for technology in *technologies*)
           (when (equal name (tech-name technology))
             (finish))
           (collecting technology)))
