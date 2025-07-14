local util = {}

function util.os()
	return package.config:sub(1, 1) == "\\" and "win" or "unix"
end

return util
