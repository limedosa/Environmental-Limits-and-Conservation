get_incident_rad = function(S.global, Tilt, Aspect, Elev, Azi){
    
    #Calculate radiation falling directly on PV panel
    #
    #   Inputs:
    #       S.global = global solar radiation (W/m2) from flat pyranometer
    #       Tilt = tilt angle of PV panel from horizontal
    #       Aspect = aspect angle of PV panel from north
    #       Elev = solar elevation angle
    #       Azi = solar azimuth angle
    #       
    #   Mininum solar elev is fixed at 5: assumes all radiation diffuse 
    #
    
    MinElev = 5
    
    x = 0.0174533 #used to covert degrees to radians
    
    
    #Parameters for diffuse fraction equation (largely guessed)
    MinDif = .15   #minimum diffuse radiation fraction
    a = 0.5        #decay parameter
    
    if (Elev >= MinElev){
        
        #Fraction diffuse radiation
        frac.dif = MinDif + (1-MinDif)*exp(-a*Elev)
    } else {
        frac.dif = 1
    }
    
    #Diffuse radiation
    S.dif = S.global*frac.dif
    
    #Horizontal radiation (direct)
    S.hor = S.global-S.dif
    
    S.inc = S.hor / sin(Elev*x)
    
    S.module = S.dif + S.inc*(cos(Elev*x)*sin(Tilt*x)*cos((Aspect - Azi)*x)+sin(Elev*x)*cos(Tilt*x))
    
    if (S.module < 0) S.module = 0
    
    return(S.module)
}


sun_elevation = function(DOY, Hour, Lat=42.3, Lon=-71.3, delGMT=-5 ){
    
    #Calcualtes solar data for locations / times.
    #
    #   Default lat/lon/timezone values set for Wellesley, MA
    #
    #   Inputs:
    #       Hour = local time of day (decimal, 24-hour clock)
    #       DOY = day of year
    #
    #   Returns:
    #       Elev = solar elevation angle
    #       Azi = solar azimuhth angle
    #       Dec = declination
    #
    #   Calculations from pveducation.org
    
    x = 0.0174533 #used to covert degrees to radians
 
    LSTM = 15*delGMT
    B = (360/365)*(DOY-81)
    EoT = 9.87*sin(2*B*x)-7.53*cos(B*x)-1.5*sin(B*x)
    TC = 4*(Lon-LSTM)+EoT
    LST = Hour+(TC/60)
    HRA = 15*(LST-12)
    Dec = 23.45*sin(B*x) #result is in degrees
    Elev = asin(sin(Dec*x)*sin(Lat*x)+cos(Dec*x)*cos(Lat*x)*cos(HRA*x))/x #in degrees
    Azi = acos((sin(Dec*x)*cos(Lat*x)-cos(Dec*x)*sin(Lat*x)*cos(HRA*x)) / cos(Elev*x))/x #in degrees
    AM = 1/cos((90-Elev)*x)
    
    if (HRA > 0) Azi = 360-Azi
    
    return(Elev)
}



sun_azimuth = function(DOY, Hour, Lat=42.3, Lon=-71.3, delGMT=-5 ){
    
    #Calcualtes solar data for locations / times.
    #
    #   Default lat/lon/timezone values set for Wellesley, MA
    #
    #   Inputs:
    #       Hour = local time of day (decimal, 24-hour clock)
    #       DOY = day of year
    #
    #   Returns:
    #       Elev = solar elevation angle
    #       Azi = solar azimuhth angle
    #       Dec = declination
    #
    #   Calculations from pveducation.org
    
    x = 0.0174533 #used to covert degrees to radians
    
    LSTM = 15*delGMT
    B = (360/365)*(DOY-81)
    EoT = 9.87*sin(2*B*x)-7.53*cos(B*x)-1.5*sin(B*x)
    TC = 4*(Lon-LSTM)+EoT
    LST = Hour+(TC/60)
    HRA = 15*(LST-12)
    Dec = 23.45*sin(B*x) #result is in degrees
    Elev = asin(sin(Dec*x)*sin(Lat*x)+cos(Dec*x)*cos(Lat*x)*cos(HRA*x))/x #in degrees
    Azi = acos((sin(Dec*x)*cos(Lat*x)-cos(Dec*x)*sin(Lat*x)*cos(HRA*x)) / cos(Elev*x))/x #in degrees
    AM = 1/cos((90-Elev)*x)
    
    if (HRA > 0) Azi = 360-Azi
    
    return(Azi)
}


