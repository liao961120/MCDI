import os
from pathlib import Path
from shlex import join  # for quoting file paths


# Additional setup for running with git-bash on Windows
if os.name == 'nt':
    from snakemake.shell import shell
    shell.executable(r'C:\Users\tomliao\AppData\Local\Programs\Git\bin\bash.exe')
    shell.prefix("""
    # Load bash predefined functions
    lastwd=$(pwd)
    source ~/.bash_profile
    cd "$lastwd"
""")

# Protect Raw data (read-only permissions for all files in `raw/`)
# from stat import S_IREAD, S_IRGRP, S_IROTH
# for fp in Path("raw").rglob("*"):
#     if fp.is_file():
#         os.chmod(fp, S_IREAD|S_IRGRP|S_IROTH)


# 6. Collect model-based estimates of AoA into data
rule model_based_analysis:
    input:
        script = "src/model_based_analysis.R"
    output:
        "made/MCDI.age.m-produciton.xlsx"
    shell:
        """
        Rscript {input.script}
        """

# 5. Fit model
rule fit_stan:
    input:
        script = "src/fit_stan.R",
        model = "src/model.stan",
        p = "made/MCDI.age-production.csv",
        u = "made/MCDI.age-understanding.csv"
    output:
        p = [
            "made/dat.fit-production.RDS",
            "made/model-production.RDS"
        ],
        # u = [
        #     "made/dat.fit-understanding.RDS",
        #     "made/model-understanding.RDS"
        # ]
    shell:
        """
        Rscript {input.script} {input.p} {output.p} 48   # production
        """
        # Rscript {input.script} {input.u} {output.u} 48   # understanding        
        
# 4. Plot empirical distribution per word
rule distrbution_per_word:
    input:
        src = "src/distribution_per_word.R"
    output:
        "made/distribution_per_word-understanding.pdf",
        "made/distribution_per_word-production.pdf"
    shell:
        """
        Rscript {input.src} UNDERSTANDING
        Rscript {input.src} PRODUCTION
        """

# 3. Compute data for estimating AoA (understanding)
rule age_distribution_understanding:
    input:
        src = "src/age_distribution_understanding.R",
        data = [
            "made/word_id.csv",
            "raw/wordbank_instrument_data_MandarinWG.csv",
        ]
    output:
        "made/MCDI.age-understanding.csv"
    shell:
        """
        Rscript {input.src}
        """

# 2. Compute data for estimating AoA (production)
rule age_distribution_production:
    input:
        src = "src/age_distribution_production.R",
        data = [
            "made/word_id.csv",
            "raw/wordbank_instrument_data_MandarinWG.csv",
            "raw/wordbank_instrument_data_MandarinWS.csv"
        ]
    output:
        "made/MCDI.age-production.csv"
    shell:
        """
        Rscript {input.src}
        """

# 1. Word metadata across WG & WS forms
rule word_id:
    input:
        src = "src/word_id.R"
    output:
        "made/word_id.csv"
    shell:
        """
        Rscript {input.src}
        """







########################################
"""
[Tips]
    DO NOT use Snakmake built-in wildcards
    lots of limitations & changes the default behaviour in for loops
    of run: blocks

[How relative paths work in Snakefile]

`input:`, `output:`, and `shell:` have the project root as the working dir.
All other directives (e.g. `script:`, `include:`, and `notebook:`) have 
the directory where `Snakefile` is located as the working dir.
"""