## <u>CSUF Pitcher Reports</u>

Pitcher Report Script which outputs a shiny app with PDF upload button. 


**Plots**
---
**Pitch Movement Plot**
  - Plots IVB and HB of each pitch type, along with 80th percentile oval.
 
**Arm Angle Plot**
  - Creates arm angle calculation using D1PitcherHeights.csv to provide accurate arm angles for pitchers.
  
**Strike Zone vs. LHH and RHH**
  - Provides 9-pocket type strike zone, with rectangles for Meatball, Waste, and Edge-Expand.
      - Meatball: 5 inches vertically and 8 inches horizontally from center, about half of the area of the strike zone.
      - Edge-Expand: All pitches that are not meatballs, but still have above a 5% chance of a swing or called strike.
      - Waste: All pitches that have below a 5% chance of being swung at or being called a strike.

**Pitch Trajectories vs. LHH and RHH**
  - Uses 9-pitch parameters to calculate 3d trajectory of a pitch from an umpire perspective.

**Tables**
---
**Top Data Table with full outing metrics and percentiles:**
  - Pitches: Total pitch count of outing.
  - Swing%: Percent of pitches swing at.
  - Whiff%: Percent of swings that were missed.
  - Chase%: Percent of pitches out of the strike zone that were swung at.
  - FPS%: Percentage of first pitches of a plate appearance that were strikes or put in play.
  - Put Away%: Percentage of 2-strike counts that ended in a strikeout.
  - Meatball%: Percentage of pitches in the Meatball Zone.
  - Edge-Expand%: Percentage of pitches in the Edge-Expand zone.
  - Waste%: Percentage of pitches in the waste zone.
 
 **Bottom Data Table with pitch-by-pitch metrics.**
  - Pitch: Pitch type. 
  - Usage: Percentage of usage for that pitch type during the outing.  
  - Velo: Average velocity of that pitch during the outing.  
  - Max Velo: Maximum velocity of that pitch during the outing.
  - IVB: Average induced vertical break of that pitch during the outing.
  - HB: Average horizontal break of that pitch during the outing.
  - Spin Dir: Average spin direction of that pitch during the outing.
  - Rel H: Average release height of the pitch during the outing.
  - Arm Ang: Average arm angle of that pitch during the outing.
  - Top VAA: Maximum vertical approach angle thrown in the zone during the outing.
  - Bot VAA: Minimum vertical approach angle thrown in the zone during the outing.
  - Extension: Average extension of that pitch during the outing.
  - Strike%: Percentage of that pitch thrown in the zone.
  - Whiff%: Percentage of swings on that pitch which resulted in a swing and miss.
  - CSW%: Called strike% + Whiff% of that pitch during the outing.
  - Chase%: Percentage of this pitch type thrown out of the zone that was swung at.
  - Stuff+: Modeled effectiveness of pitch characteristics independent of location.
    - 100 is average, every standard deviation is &plusmn; 10.
     
  



  <img width="1107" height="856" alt="Screenshot 2026-05-01 at 1 54 51 PM" src="https://github.com/user-attachments/assets/dd16ca03-72ef-40c0-bef8-d2d539711018" />
