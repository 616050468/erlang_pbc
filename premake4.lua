--
-- premake4 file to build RecastDemo
-- http://industriousone.com/premake
--

local action = _ACTION or ""
local todir = "Build/" .. action

solution "pbc_nif"
    configurations { 
        "Debug",
        "Release"
    }
    location (todir)

    -- extra warnings, no exceptions or rtti
    flags { 
        "FloatFast",
        "NoExceptions",
        "Symbols"
    }

    -- debug configs
    configuration "Debug*"
        defines { "DEBUG" }
        targetdir ( todir .. "/lib/Debug" )
 
    -- release configs
    configuration "Release*"
        defines { "NDEBUG" }
        flags { "Optimize" }
        targetdir ( todir .. "/lib/Release" )

    configuration {"linux", "gmake"}
        defines { "_CRT_SECURE_NO_WARNINGS","CODE_INLINE", "KBE_USE_ASSERTS"}
    -- windows specific
    configuration {"windows"}
        defines { "_CRT_SECURE_NO_WARNINGS","CODE_INLINE", "KBE_USE_ASSERTS"}

project "pbc"
    language "C++"
    kind "StaticLib"
    includedirs {
        ".",
        "pbc"
    }
    files { 
        "pbc/*.h",
        "pbc/*.c"
    }

project "pbc_nif"
    language "C++"
    kind "SharedLib"
    includedirs {
        ".",
        "pbc",
        "pbc_nif",
        "/usr/lib/erlang/usr/include"
    }
    files {
        "./*.h",
        "pbc_nif/*.h",
        "pbc_nif/*.c",
        "pbc_nif/*.cpp"
    }
    -- project dependencies
    links { 
        "pbc"
    }
