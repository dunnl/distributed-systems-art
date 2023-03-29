def command?(command)
  system("which #{command} > /dev/null 2>&1")
end

if ENV['generator']
  $builder = ENV['generator']
  print("Using builder $builder")
elsif command?("generator") then
  $builder = "generator"
else
  $builder = "cabal run art-gen --"
end

def go (dir, selections, sizes)
  Dir.mkdir dir unless File.directory? dir
  fdout = File.open("#{dir}/output.txt", "a")
  fderr = File.open("#{dir}/error.txt", "a")
  selections.each do |sel|
    sizes.each do |size|
      command = "#{$builder} --selection #{sel} --output #{dir}/#{sel}.png --width #{size}"
      Kernel.spawn(command, :out => fdout, :err => fderr)
    end
  end
  fdout.close
  fderr.close
end

def mkImages
  selections = ["request", "externalorder", "partialorder", "linear1", "linear2", "linear3", "sequential1", "sequential2", "causal1"]
  sizes = ["1024"]
  go("_out", selections, sizes)
end

mkImages
