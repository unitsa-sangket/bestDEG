# SecretoFinder: A Snakemake workflow for identifying animal secretory proteins


# Installing SecretoFinder

## Step 1: Installing Miniconda 3

This installation requires the 64-bit operating system and install the miniconda via the terminal. Simply type

```bash
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
bash Miniconda3-latest-Linux-x86_64.sh
```
Please answer "yes" for the question "Do you wish the installer to initialize Miniconda3 by running conda init? [yes|no]".
Then relogin.
For more information of conda installation, please see https://docs.conda.io/en/latest/miniconda.html#linux-installers

## Step 2: Installing Snakemake

Install python and pip before installing snakemake.

```bash
conda install -c anaconda python=3.6
conda install -c anaconda pip
conda activate base
pip install snakemake==4.3.1
```

## Step 3: Installing Deeploc

Install deeploc dependencies (https://github.com/Lasagne/Lasagne).

```bash
pip install -r https://raw.githubusercontent.com/Lasagne/Lasagne/master/requirements.txt
pip install https://github.com/Lasagne/Lasagne/archive/master.zip
```

Download deeploc program from https://services.healthtech.dtu.dk/service.php?DeepLoc-1.0 and install deeploc using python.
  
```bash
tar -xvzf deeploc-1.0.All.tar.gz
cd deeploc-1.0
python setup.py install
```

## Step 4: Installing SignalP

Download deeploc program from https://services.healthtech.dtu.dk/service.php?SignalP-5.0.
  
```bash
tar -xvzf signalp-5.0b.Linux.tar.gz
```
Export path of executable file   (https://linuxize.com/post/how-to-add-directory-to-path-in-linux/).
Open ".bashrc" file with your text editor.

```bash
nano ~/.bashrc
```

Add the executable path of signalp at the end of the file.

```bash
export PATH="/home/username/signalp-5.0b/bin:$PATH"
```

Then execute the file.

```bash
source ~/.bashrc
```

## Step 5: Installing extract_fasta_seq

```bash
conda install -c bioconda extract_fasta_seq
```

## Step 6: Installing Rtsne and ggplot2 packages

Run R and install Rtsne and ggplot2

```bash
R
install.packages("Rtsne")
install.packages("ggplot2")
```

## Step 7: Downloading workflow, software, and prepare working directory

Download the SecretoFinder workflow and an example of input file. 
```bash
git clone https://github.com/JirathNuan/SecretoFinder.git
cd SecretoFinder
```

Then the SecretoFinder is ready to use!


# Running SecretoFinder

Put an input file in fasta format to your working directory.
The SecretoFinder is developed based on the Snakemake workflow management system. Therefore, to run SecretoFinder, simply type

```bash
snakemake
```
Then the workflow will process automatically.
The output files contain signalp_summary.signalp5, query_SP-containing-proteins.txt, SP-containing-proteins.fasta, deeploc_output.txt, figure_tSNE_deeploc.png
Please see the example of input and output files in executed_example folder.
