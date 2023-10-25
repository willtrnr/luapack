local __luapack<const> = {preload = {}, loaded = {}}

setmetatable(__luapack.preload, {
   __index = function(_t, k)
      local v = _G[k]
      if v ~= nil then
         return function() return v end
      else
         return nil
      end
   end
})

local require<const> = function(modname)
   local mod = __luapack.loaded[modname]
   if mod ~= nil then return mod end

   local loader = __luapack.preload[modname]
   if load ~= nil then
      mod = loader(modname, ":preload:")
      if mod == nil then
         mod = __luapack.loaded[modname]
         if mod == nil then mod = true end
      end
      __luapack.loaded[modname] = mod
      return mod
   end

   error("module '" .. modname .. "' not found")
end
