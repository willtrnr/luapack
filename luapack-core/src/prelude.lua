local __luapack <const> = { preload = {}, loaded = {} }

local require <const> = function(modname)
   local mod = __luapack.loaded[modname]
   if mod ~= nil then
      return mod
   end
   local loader = __luapack.modules[modname]
   if load ~= nil then
      mod = loader(modname, ":preload:")
      if mod == nil then
         mod = __luapack.loaded[modname]
         if mod == nil then
            mod = true
         end
      end
      __luapack.loaded[modname] = mod
      return mod
   end
   error("module '" .. modname .. "' not found")
end
