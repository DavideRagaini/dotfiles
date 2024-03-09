local mp = require 'mp' -- isn't actually required, mp still gonna be defined

local opts = {
  leader_key = ',',
  pause_on_open = false,
  resume_on_exit = "only-if-was-paused", -- another possible value is true
  hide_timeout = 2, -- timeout in seconds to hide menu
  which_key_show_delay = 0.1, -- timeout in seconds to show which-key menu
  strip_cmd_at = 28, -- max symbols for cmd names in which-key menu

  -- styles
  font_size = 21,
  menu_x_padding = 3, -- this padding for now applies only to 'left', not x
  which_key_menu_y_padding = 3,
}

(require 'mp.options').read_options(opts, mp.get_script_name())

package.path =
mp.command_native({ "expand-path", "~~/script-modules/?.lua;" }) .. package.path

local leader = require "leader"

leader:init(opts) -- binds leader key

mp.register_script_message("leader-bindings-request", function()
  leader:provide_leader_bindings()
end)

-- FIXME: need timeout below since we need all functions to be defined before
-- this script will run. Trying to call init() with this timeout won't work
-- since it's gonna pause loading of all other scripts.

-- another more reliable, but longer way to set leader bindings after all
-- scripts have loaded is to set a timer, threshold and set an observer on
-- 'input-bindings' mpv prop and run timer each time this prop gets updated. And
-- if timer passes threshold - run 'set_leader_bindings'

mp.add_timeout(0.3, function()
  leader:set_leader_bindings(
  -- key, name (must be unique!), comment, [follower bindings]
    {
      { 'x', 'M-x', 'execute-extended-command' },
      { 't', 'script-message-to console type "set time-pos "', 'time-pos' },

      { '<', 'seek -60', 'seek 1 minute backward' },
      { '>', 'seek 60', 'seek 1 minute forward' },
      -- { '.', 'frame-step', 'advance one frame and pause' },
      -- { ',', 'frame-back-step', 'go back by one frame and pause' },
      { 'N', 'playlist-next', 'skip to the next file' },
      { 'P', 'playlist-previous', 'skip to the previous file' },
      { 'b', 'script-binding sponsorblock_minimal/toggle', 'toggle sponsorblock' },

      { 'l', 'ab-loop', 'set/clear A-B loop points' },
      { 'L', 'cycle-values loop-file "inf" "no"', 'toggle infinite looping' },
      { '.', 'show-text ${playlist}', 'show the playlist' },
      { '/', 'show-text ${track-list}', 'show the list of video, audio and sub tracks' },
      { ',', 'script-message osc-chapterlist 4 ; show-text "${osd-ass-cc/0}{an9}${osd-ass-cc/1}${chapter-list}"', 'show chapterlist' },
      { 'q', 'quit-watch-later', 'exit and remember the playback position' },

      { 'a', 'prefix', 'audio', {
        { '+', 'add audio-delay 0.100', 'change audio/video sync by delaying the audio' },
        { '-', 'add audio-delay -0.100', 'change audio/video sync by shifting the audio' },
        { '=', 'script-message-to console type "set audio-delay "', 'set audio delay (ms)'  },
        { 'I', 'cycle-values af dynaudnorm=f=75:g=25:p=0.55 dynaudnorm=f=400:g=5:m=4:p=0.95:t=0 loudnorm=I=-16:TP=-3:LRA=4 loudnorm=I=-30 anull', 'cycle dynadnorm' },
        { 'a', 'toggle-acompressor', 'toggle acompressor' },
        { 'c', 'cycle audio', 'switch audio track' },
        { 'i', 'cycle-values af loudnorm=i=-15 loudnorm=i=-30 anull', 'cycle integral loudnorm' },
        { 'l', 'script-message-to command_palette show-command-palette audio', 'audio tracks list' }
      } },

      { 'c', 'prefix', 'chapters', {
        { 'm', 'chapters-menu', 'open current video chapters' },
        { 'n', 'add chapter 1', 'seek to the next chapter' },
        { 'p', 'add chapter -1', 'seek to the previous chapter' },
        { 'l', 'script-message-to command_palette show-command-palette chapters', 'chapters list' },
      } },

      { 'f', 'prefix', 'filters', {
        { 'b', 'prefix', 'brightness', {
          { '+', 'add brightness 1' },
          { '-', 'add brightness -1' },
          { '=', 'script-message-to console type "set brightness "', 'set brightness (-100<x<100)'},
          { 'r', 'set brightness 0' }
        } },
        { 'c', 'prefix', 'contrast', {
          { '+', 'add contrast 1' },
          { '-', 'add contrast -1' },
          { '=', 'script-message-to console type "set contrast "', 'set contrast (-100<x<100)'},
          { 'r', 'set contrast 0' }
        } },
        { 'd', 'cycle deband', 'toggle deband' },
        { 'g', 'prefix', 'gamma', {
          { '+', 'add gamma 1' },
          { '-', 'add gamma -1' },
          { '=', 'script-message-to console type "set gamma "', 'set gamma (-100<x<100)'},
          { 'r', 'set gamma 0' }
        } },
        { 'r', 'set contrast 0; set brightness 0; set gamma 0; set saturation 0; show-text "filter reset"', 'reset filters' },
        { 's', 'prefix', 'saturation', {
          { '+', 'add saturation 1' },
          { '-', 'add saturation -1' },
          { '=', 'script-message-to console type "set saturation "', 'set saturation (-100<x<100)'},
          { 'r', 'set saturation 0' }
        } },

        { 'h', 'prefix', 'shaders', {
          { 'R', 'show-text "Shaders: ${glsl-shaders}"' },
          { 'i', 'cycle-values glsl-shaders toggle "~~/shaders/invert.glsl"', 'invert color shader' },
          { 'l', 'cycle-values glsl-shaders toggle "~~/shaders/LumaSharpenHook.glsl"', 'LumaSharpenHook shader' },
          { 'r', 'set gamma 0' },
        } },
      } },

      { 'o', 'prefix', 'command_palette', {
        { 'C', 'show-text command; script-message-to command_palette show-command-palette commands', 'commands list' },
        { 'P', 'show-text properties; script-message-to command_palette show-command-palette properties', 'properties list' },
        { 'a', 'show-text audio; script-message-to command_palette show-command-palette audio', 'audio list' },
        { 'b', 'show-text bindings; script-message-to command_palette show-command-palette bindings', 'bindings list' },
        { 'c', 'show-text chapters; script-message-to command_palette show-command-palette chapters', 'chapters list' },
        { 'l', 'show-text profiles; script-message-to command_palette show-command-palette profiles', 'profiles list' },
        { 'o', 'show-text options; script-message-to command_palette show-command-palette options', 'options list' },
        { 'p', 'show-text playlist; script-message-to command_palette show-command-palette playlist', 'playlist list' },
        { 's', 'show-text subtitle; script-message-to command_palette show-command-palette subtitle', 'subtitles list' },
        { 'v', 'show-text video; script-message-to command_palette show-command-palette video', 'video list' },
      } },

      { 'u', 'prefix', 'undo', {
        { 'l', 'script-binding UndoRedo/undoLoop', 'loop' },
        { 'L', 'script-binding UndoRedo/undoLoopCaps', 'undoLoopCaps' },
        { 'r', 'script-binding UndoRedo/redo', 'redo' },
        { 'R', 'script-binding UndoRedo/redoCaps', 'redoCaps' },
        { 'u', 'script-binding UndoRedo/undo', 'undo' },
        { 'U', 'script-binding UndoRedo/undoCaps', 'undoCaps' },
      } },

      { 's', 'prefix', 'subtitles', {
        { 'a', 'prefix', 'subtitle align', {
            { 'C', 'set sub-align-x center' },
            { 'b', 'set sub-align-y bottom' },
            { 'c', 'set sub-align-y center' },
            { 'l', 'set sub-align-x left' },
            { 'r', 'set sub-align-x right' },
            { 't', 'set sub-align-y top' },
        } },
        { 'f', 'prefix', 'sub font', {
          { '+', 'add sub-scale +0.1' },
          { '-', 'add sub-scale -0.1' },
          { '=', 'script-message-to console type "set sub-scale "', 'set sub-scale (0<x<5)'  },
          { 'C', 'set sub-font \"Cabin-f\"' },
          { 'R', 'set sub-font \"RobotoMono Nerd Font Propo SemiBold\"' },
          { 'c', 'set sub-font \"Cabin\"' },
          { 'd', 'set sub-font \"DejavuSans\"' },
          { 'g', 'set sub-font \"Gandhi Sans\"' },
          { 'r', 'set sub-scale 1' },
          { 's', 'script-message-to console type "set sub-font-size "', 'set sub-scale (8<x<30)'  },
        } },
        { '+', 'add sub-delay +0.1', 'shift subtitles 100 ms' },
        { '-', 'add sub-delay -0.1', 'shift subtitles 100 ms earlier' },
        { '=', 'script-message-to console type "set sub-delay "', 'set sub delay' },
        { 'O', 'cycle-values sub-ass-override "force" "no"', 'toggle overriding SSA/ASS subtitle styles with the normal styles' },
        { 'd', 'add sub-pos 1', 'subtitle move down' },
        { 'l', 'script-message-to command_palette show-command-palette subtitle', 'subtitles list' },
        { 'n', 'sub-seek 1', 'seek to the previous subtitle' },
        { 'o', 'cycle sub', 'switch subtitle track' },
        { 'p', 'sub-seek -1', 'seek to the next subtitle' },
        { 's', 'cycle sub-ass-vsfilter-aspect-compat', 'toggle stretching SSA/ASS subtitles with anamorphic videos to match the historical renderer' },
        { 't', 'cycle sub-visibility', 'hide or show the subtitles' },
        { 'u', 'add sub-pos -1', 'subtitle move up' },
      } },

      { 'n', 'prefix', 'channel_mixer', {
          { 'c', 'script-binding channel_mixer/mix("cmCenter",0.1)', 'Center +0.1' },
          { 'C', 'script-binding channel_mixer/mix("cmCenter",-0.1)', 'Center -0.1' },
          { 'f', 'script-binding channel_mixer/mix("cmFront",0.1)', 'Front +0.1' },
          { 'F', 'script-binding channel_mixer/mix("cmFront",-0.1)', 'Front -0.1' },
          { 's', 'script-binding channel_mixer/mix("cmSide",0.1)', 'Side +0.1' },
          { 'S', 'script-binding channel_mixer/mix("cmSide",-0.1)', 'Side -0.1' },
          { 'b', 'script-binding channel_mixer/mix("cmBack",0.1)', 'Back +0.1' },
          { 'B', 'script-binding channel_mixer/mix("cmBack",-0.1)', 'Back -0.1' },
          { 'l', 'script-binding channel_mixer/mix("cmLFE",0.1)', 'LFE +0.1' },
          { 'L', 'script-binding channel_mixer/mix("cmLFE",-0.1)', 'LFE -0.1' },
      } },

      { 'm', 'prefix', 'acompressor', {
        { 'e', 'show-text toggle;script-binding acompressor/toggle-acompressor', 'toggle' },
        { 't', 'show-text threshold up;script-binding acompressor/acompressor-increase-threshold', 'increase-threshold' },
        { 'T', 'show-text threshold down;script-binding acompressor/acompressor-decrease-threshold', 'decrease-threshold' },
        { 'r', 'show-text ratio up;script-binding acompressor/acompressor-increase-ratio', 'increase-ratio' },
        { 'R', 'show-text ratio down;script-binding acompressor/acompressor-decrease-ratio', 'decrease-ratio' },
        { 'k', 'show-text knee up;script-binding acompressor/acompressor-increase-knee', 'increase-knee' },
        { 'K', 'show-text knee down;script-binding acompressor/acompressor-decrease-knee', 'decrease-knee' },
        { 'm', 'show-text makuep up;script-binding acompressor/acompressor-increase-makeup', 'increase-makeup' },
        { 'M', 'show-text makuep down;script-binding acompressor/acompressor-decrease-makeup', 'decrease-makeup' },
        { 'a', 'show-text attack up;script-binding acompressor/acompressor-increase-attack', 'increase-attack' },
        { 'A', 'show-text attack down;script-binding acompressor/acompressor-decrease-attack', 'decrease-attack' },
        { 'h', 'show-text release up;script-binding acompressor/acompressor-increase-release', 'increase-release' },
        { 'H', 'show-text release down;script-binding acompressor/acompressor-decrease-release', 'decrease-release' },
      } },

      { 'k', 'prefix', 'skipsilence', {
        { 'I', 'script-binding skipsilence/cycle-info-up', 'cycle-info-up' },
        { 'a', 'script-binding skipsilence/toggle-arnndn', 'toggle-arnndn' },
        { 'd', 'script-binding skipsilence/disable', 'disable' },
        { 'e', 'script-binding skipsilence/enable', 'enable' },
        { 'i', 'script-binding skipsilence/info', 'info' },
        { 'j', 'script-binding skipsilence/threshold-down', 'threshold-down' },
        { 'k', 'script-binding skipsilence/threshold-up', 'threshold-up' },
        { 'o', 'script-binding skipsilence/toggle-arnndn-output', 'toggle-arnndn-output' },
        { 'r', 'script-binding skipsilence/reset-total', 'reset-total' },
        { 't', 'script-binding skipsilence/toggle', 'toggle' },
      } },

      { 'p', 'prefix', 'playback', {
        { '1', 'set speed 1.25', 'set 1.25 playback speed' },
        { '2', 'set speed 1.50', 'set 1.50 playback speed' },
        { '3', 'set speed 1.75', 'set 1.75 playback speed' },
        { '4', 'set speed 2.00', 'set 2.25 playback speed' },
        { '5', 'set speed 2.50', 'set 2.50 playback speed' },
        { '6', 'set speed 3.00', 'set 3.00 playback speed' },
        { 'a', 'script-binding quality_menu/audio_formats_toggle', 'quality_menu/audio_formats_toggle' },
        { 'p', 'set speed 2.00; script-binding skipsilence/enable', 'skipsilence/toggle' },
        { 'v', 'script-binding quality_menu/video_formats_toggle', 'quality_menu/video_formats_toggle' },
      } },

      { 'y', 'prefix', 'yank', {
        { 'd', 'script-message-to copyStuff copyDuration', 'copy duration [HH:MM:SS]' },
        { 'f', 'script-message-to copyStuff copyFilename', 'copy filename' },
        { 'm', 'script-message-to copyStuff copyMetadata', 'copy matadata [yt.description/file.metadata]' },
        { 'o', 'script-message-to copyStuff copyOrg', 'copy org link [[link][description]]' },
        { 'p', 'script-message-to copyStuff copyFullPath', 'copy full path [url/path]' },
        { 'r', 'script-message-to copyStuff copyRelativePath', 'copy relative path [directory/filename]' },
        { 's', 'script-message-to copyStuff copySubtitle', 'copy subtitle' },
        { 't', 'script-message-to copyStuff copyTime', 'copy time [HH:MM:SS.3ms]' },
      } }
    }
  )
end)
