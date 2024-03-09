cmCenter = 1
local defCenter = 1
cmFront = 0.707
local defFront = 0.707
cmSide = 0.707
local defSide = 0.707
cmBack = 0.707
local defBack = 0.707
cmLFE = 0
local defLFE = 0

local function mix(channel, adjustment)
	if channel == "reset" then
		cmCenter = defCenter
		cmFront = defFront
		cmSide = defSide
		cmBack = defBack
		cmLFE = defLFE
	else
		_G[channel] = _G[channel] + adjustment
		if _G[channel] < 0.001 then _G[channel] = 0 end
	end
	mp.set_property("af", "lavfi=[pan=stereo|FL="..cmCenter.."FC+"..cmFront.."FL+"..cmSide.."SL+"..cmBack.."BL+"..cmLFE.."LFE|FR="..cmCenter.."FC+"..cmFront.."FR+"..cmSide.."SR+"..cmBack.."BR+"..cmLFE.."LFE]")
	mp.osd_message("lavfi=[pan=stereo|\nFL="..cmCenter.."FC+"..cmFront.."FL+"..cmSide.."SL+"..cmBack.."BL+"..cmLFE.."LFE|\nFR="..cmCenter.."FC+"..cmFront.."FR+"..cmSide.."SR+"..cmBack.."BR+"..cmLFE.."LFE]", 5)
end

function() mix("reset")

mp.add_key_binding("F12", "mReset", function() mix("reset") end)

mp.add_key_binding("Alt+Ctrl+c", "cUp", function() mix("cmCenter",0.1) end)
mp.add_key_binding("Alt+Shift+c", "cDown", function() mix("cmCenter",-0.1) end)

mp.add_key_binding("Alt+Ctrl+f", "fUp", function() mix("cmFront",0.1) end)
mp.add_key_binding("Alt+Shift+f", "fDown", function() mix("cmFront",-0.1) end)

mp.add_key_binding("Alt+Ctrl+s", "sUp", function() mix("cmSide",0.1) end)
mp.add_key_binding("Alt+Shift+s", "sDown", function() mix("cmSide",-0.1) end)

mp.add_key_binding("Alt+Ctrl+b", "bUp", function() mix("cmBack",0.1) end)
mp.add_key_binding("Alt+Shift+b", "bDown", function() mix("cmBack",-0.1) end)

mp.add_key_binding("Alt+Ctrl+l", "lUp", function() mix("cmLFE",0.1) end)
mp.add_key_binding("Alt+Shift+l", "lDown", function() mix("cmLFE",-0.1) end)
