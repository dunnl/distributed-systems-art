def command?(command)
  system("which #{command} > /dev/null 2>&1")
end

# Check if the generator command is in the path. If so we are probably
# in a Nix build shell with the generator as a buildInput
if command?("generator") then
  $builder = "generator"
# Else check if the generator is passed as an environmental variable and use that
elsif ENV['generator']
  $builder = ENV['generator']
else
  # Else check try running the generator with cabal (useful for local development)
  $builder = "cabal run generator --"
  # Actually, don't do this. If it has been built locally already, it will crash the system."
  # print "Refusing to run \"cabal run generator\" to prevent the system from hanging"
  # exit 1
end

print "make.rb: Set builder to \"#{$builder}\"\n"

def go (dir, selections, sizes)
  Dir.mkdir dir unless File.directory? dir
  fdout = File.open("#{dir}/output.txt", "a")
  fderr = File.open("#{dir}/error.txt", "a")
  selections.each do |sel|
    sizes.each do |size|
      print "Creating #{sel}\n"
      command = "#{$builder} --selection #{sel} --output #{dir}/#{sel}.pgf"
      Kernel.system(command, :out => fdout, :err => fderr)
    end
  end
  fdout.close
  fderr.close
end

def mkImages
  selections = [ "mpEx1", "mpEx1Sc", "mpEx1Vec", "mpEx1Mat",
                 "mpEx2", "mpEx2Sc", "mpEx2Vec", "mpEx2Mat",
                 "mpEx3", "mpEx3Sc", "mpEx3Vec", "mpEx3Mat",
               ]
  sizes = ["1024"]
  go("_out", selections, sizes)
end

mkImages
