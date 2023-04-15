# Based on documentation of DFControl and DFTK
using LibGit2: LibGit2
using Pkg: Pkg

# To manually generate the docs:
# Run "julia make.jl" to generate the docs

# Set to true to disable some checks and cleanup
DEBUG = false

# Where to get files from and where to build them
SRCPATH = joinpath(@__DIR__, "src")
BUILDPATH = joinpath(@__DIR__, "build")
ROOTPATH = joinpath(@__DIR__, "..")
CONTINUOUS_INTEGRATION = get(ENV, "CI", nothing) == "true"

JLDEPS = [Pkg.PackageSpec(;
                          url = "https://github.com/louisponet/Overseer.jl.git",
                          rev = LibGit2.head(ROOTPATH))]

# Setup julia dependencies for docs generation if not yet done
Pkg.activate(@__DIR__)
Pkg.develop(Pkg.PackageSpec(; path = ROOTPATH))
Pkg.instantiate()

# Setup environment for making plots
ENV["GKS_ENCODING"] = "utf8"
ENV["GKSwstype"] = "100"
ENV["PLOTS_TEST"] = "true"

# Import packages for docs generation
using Overseer
using Documenter
using Literate

usings = quote
    using Overseer
end 

DocMeta.setdocmeta!(Overseer, :DocTestSetup, usings; recursive=true)

EXAMPLES = [String(m[1])
            for m in match.(r"\"(examples/[^\"]+.md)\"", readlines(joinpath(SRCPATH, "index.md"))) if !isnothing(m)]

literate_files = NamedTuple[(src  = joinpath(ROOTPATH, splitext(file)[1] * ".jl"),
                   dest = joinpath(SRCPATH, "examples"),
                   example = true) for file in EXAMPLES]
                   
for (dir, directories, files) in walkdir(SRCPATH)
    for file in files
        if endswith(file, ".jl")
            push!(literate_files, (src = joinpath(dir, file), dest = dir, example = false))
        end
    end
end

for file in literate_files
    # preprocess = file.example ? add_badges : identity
    # Literate.markdown(file.src, file.dest; documenter=true, credit=false,
    #                   preprocess=preprocess)
    # Literate.notebook(file.src, file.dest; credit=false,
    #                   execute=CONTINUOUS_INTEGRATION || DEBUG)
    Literate.markdown(file.src, file.dest; documenter = true, credit = false)
    Literate.notebook(file.src, file.dest; credit = false,
                      execute = CONTINUOUS_INTEGRATION || DEBUG)
end

# Generate the docs in BUILDPATH
makedocs(; modules = [Overseer],
         format = Documenter.HTML(; prettyurls = CONTINUOUS_INTEGRATION,
                                  canonical = "https://louisponet.github.io/Overseer.jl/stable/",
                                  assets = ["assets/favicon.ico"]),
         sitename = "Overseer.jl", authors = "Louis Ponet",
         linkcheck_ignore = [
                             # Ignore links that point to GitHub's edit pages, as they redirect to the
                             # login screen and cause a warning:
                             r"https://github.com/([A-Za-z0-9_.-]+)/([A-Za-z0-9_.-]+)/edit(.*)"],
         pages = ["Home"            => "index.md",
                  # "Getting Started" => "installation.md",
                  "Topics"     => ["Ledger" => "ledger.md",
                                   "Components" => "components.md",
                                   "Systems"  => "systems.md",
                                   "Entities" => "entities.md",
                                   "Iteration" => "iteration.md"]])

# Deploy docs to gh-pages branch
deploydocs(; repo = "github.com/louisponet/Overseer.jl.git", devbranch = "master")

if !CONTINUOUS_INTEGRATION
    println("\nDocs generated, try $(joinpath(BUILDPATH, "index.html"))")
end
