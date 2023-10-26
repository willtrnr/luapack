local __luapack = {preload = {}, loaded = {}}

local require = function(modname)
   local mod = __luapack.loaded[modname]
   if mod ~= nil then return mod end

   local loader = __luapack.preload[modname]
   if loader ~= nil then
      mod = loader(modname, ":preload:")
      if mod == nil then
         mod = __luapack.loaded[modname]
         if mod == nil then mod = true end
      end
      __luapack.loaded[modname] = mod
      return mod
   end

   error("module '" .. modname .. "' not found", 2)
end
