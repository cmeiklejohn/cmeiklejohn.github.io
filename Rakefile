require 'erb'

desc "Create a new post in _posts"
task :post, [:title] do |t, args|
  d = Time.new
  file_date = d.strftime('%F')
  date = d.to_s
  post_erb = "_templates/post.erb"
  title = args.title
  file_title = title.split.join('-').downcase
  filename = "_posts/#{file_date}-#{file_title}.markdown"
  content = ERB.new(File.read(post_erb)).result(binding)
  File.open(filename, "w") { |file| file.write(content) }
end

desc "Build amd serve"
task :bs do |t|
  sh "jekyll build && jekyll serve"
end
