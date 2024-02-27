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
      -- Scripts
      { 'x', 'M-x', 'execute-extended-command' },
      { 't', 'script-message-to console type "set time-pos "', 'time-pos' },

      -- Playback
      { '<', 'seek -60', 'seek 1 minute backward' },
      { '>', 'seek 60', 'seek 1 minute forward' },
      -- { '.', 'frame-step', 'advance one frame and pause' },
      -- { ',', 'frame-back-step', 'go back by one frame and pause' },
      { 'N', 'playlist-next', 'skip to the next file' },
      { 'P', 'playlist-previous', 'skip to the previous file' },

      -- Other
      { 'l', 'ab-loop', 'set/clear A-B loop points' },
      { 'L', 'cycle-values loop-file "inf" "no"', 'toggle infinite looping' },
      { '.', 'show-text ${playlist}', 'show the playlist' },
      { '/', 'show-text ${track-list}', 'show the list of video, audio and sub tracks' },
      { ',', 'script-message osc-chapterlist 4 ; show-text "${osd-ass-cc/0}{an9}${osd-ass-cc/1}${chapter-list}"', 'show chapterlist' },

      { 'q', 'quit-watch-later', 'exit and remember the playback position' },

      -- Prefixes
      { 'a', 'prefix', 'audio', {
        { '=', 'script-message-to console type "set audio-delay "', 'set audio delay (ms)'  },
        { '+', 'add audio-delay 0.100', 'change audio/video sync by delaying the audio' },
        { '-', 'add audio-delay -0.100', 'change audio/video sync by shifting the audio' },
        { 'a', 'toggle-acompressor', 'toggle acompressor' },
        { 'c', 'cycle audio', 'switch audio track' },
        { 'i', 'cycle-values af loudnorm=i=-15 loudnorm=i=-30 anull', 'cycle integral loudnorm' },
        { 'I', 'cycle-values af dynaudnorm=f=75:g=25:p=0.55 dynaudnorm=f=400:g=5:m=4:p=0.95:t=0 loudnorm=I=-16:TP=-3:LRA=4 loudnorm=I=-30 anull', 'cycle dynadnorm' },
        { 'l', 'script-message-to command_palette show-command-palette audio', 'audio tracks list' }
      } },

      { 'c', 'prefix', 'chapters', {
        { 'm', 'chapters-menu', 'open current video chapters' },
        { 'n', 'add chapter 1', 'seek to the next chapter' },
        { 'p', 'add chapter -1', 'seek to the previous chapter' },
        { 'l', 'script-message-to command_palette show-command-palette chapters', 'chapters list' },
      } },

        -- { 'd', 'prefix', 'drcbox', {
        --   { 'd', 'script-message-to drcbox toggle_drcbox', 'toggle drcbox' },
        --   { 'D', 'script-message-to drcbox toggle_bindings', 'toggle drcbox bindings' },
        --   { 'r', 'script-message-to drcbox reset_drcbox', 'reset drcbox' },
        -- } },

      { 'f', 'prefix', 'filters', {
        -- example of multi-nested prefixes
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
          { 'i', 'cycle-values glsl-shaders toggle "~~/shaders/invert.glsl"', 'invert color shader' },
          { 'l', 'cycle-values glsl-shaders toggle "~~/shaders/LumaSharpenHook.glsl"', 'LumaSharpenHook shader' },
          { 'r', 'set gamma 0' },
          { 'R', 'show-text "Shaders: ${glsl-shaders}"' }
        } },
      } },

      -- { 'k', 'prefix', 'skipsilence', {
      --     { 't', 'script-message-to skipsilence toggle', 'toggle' },
      --     { 'd', 'script-message-to skipsilence function() adjust_thresholdDB(-1) end, "repeatable"', 'threshold down' },
      --     { 'u', 'script-message-to skipsilence function() adjust_thresholdDB(1) end', 'threshold down' },
      --     { 'i', 'script-message-to skipsilence info', 'info' },
      --     { 'r', 'script-message-to skipsilence reset_total_saved_time', 'reset' }
      -- } },

      { 'o', 'prefix', 'command_palette', {
        { 'b', 'script-message-to command_palette show-command-palette bindings', 'bindings list' },
        { 'C', 'script-message-to command_palette show-command-palette commands', 'commands list' },
        { 'P', 'script-message-to command_palette show-command-palette properties', 'properties list' },
        { 'o', 'script-message-to command_palette show-command-palette options', 'options list' },
        { 'p', 'script-message-to command_palette show-command-palette playlist', 'playlist list' },
        { 'c', 'script-message-to command_palette show-command-palette chapters', 'chapters list' },
        { 'a', 'script-message-to command_palette show-command-palette audio', 'audio list' },
        { 's', 'script-message-to command_palette show-command-palette subtitle', 'subtitles list' },
        { 'v', 'script-message-to command_palette show-command-palette video', 'video list' },
        { 'l', 'script-message-to command_palette show-command-palette profiles', 'profiles list' }
      } },

      { 's', 'prefix', 'subtitles', {
        { '-', 'add sub-delay -0.1', 'shift subtitles 100 ms earlier' },
        { '+', 'add sub-delay +0.1', 'shift subtitles 100 ms' },
        { '=', 'script-message-to console type "set sub-delay "', 'set sub delay' },
        { 'u', 'add sub-pos -1', 'subtitle move up' },
        { 'd', 'add sub-pos 1', 'subtitle move down' },
        { 't', 'cycle sub-visibility', 'hide or show the subtitles' },

        -- should it be here? it's more about playback section
        { 'n', 'sub-seek 1', 'seek to the previous subtitle' },
        { 'p', 'sub-seek -1', 'seek to the next subtitle' },

        { 's', 'cycle sub-ass-vsfilter-aspect-compat',
          'toggle stretching SSA/ASS subtitles with anamorphic videos to match the historical renderer' },
        { 'O', 'cycle-values sub-ass-override "force" "no"',
          'toggle overriding SSA/ASS subtitle styles with the normal styles' },
        { 'o', 'cycle sub', 'switch subtitle track' },

        { 'f', 'prefix', 'sub scale', {
          { '+', 'add sub-scale +0.1' },
          { '-', 'add sub-scale -0.1' },
          { '=', 'script-message-to console type "set sub-scale "', 'set sub-scale (0<x<5)'  },
          { 'r', 'set sub-scale 1' }
        } },
        { 'l', 'script-message-to command_palette show-command-palette subtitle', 'subtitles list' }
      } },

      { 'p', 'prefix', 'playback', {
        { '1', 'set speed 1.25', 'set 1.25 playback speed' },
        { '2', 'set speed 1.50', 'set 1.50 playback speed' },
        { '3', 'set speed 1.75', 'set 1.75 playback speed' },
        { '4', 'set speed 2.00', 'set 2.25 playback speed' },
        { '5', 'set speed 2.50', 'set 2.50 playback speed' },
        { '6', 'set speed 3.00', 'set 3.00 playback speed' },
        { 'a', 'script-binding quality_menu/audio_formats_toggle', 'quality_menu/audio_formats_toggle' },
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
