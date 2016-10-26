#!/usr/bin/env ruby

require 'open-uri'

picture_folder = File.expand_path("~/Pictures/pod")

page = open('http://cn.bing.com/', 'User-Agent' => "Mozilla/5.0 (X11; Linux x86_64; rv:45.0) Gecko/20100101 Firefox/45.0").read
img_url = page.to_s.scan(/g_img={url: \"(.*?)\",/).last.first
img_url = (img_url.start_with?('//') ? 'http' : 'http://www.bing.com') + img_url
file_name = File.join(picture_folder, img_url.scan(/([^\/]+)/).last.first)

File.open(file_name, 'wb') do |fp|
    fp.write open(img_url).read
    fp.close
end

Process.spawn('fbsetbg', file_name)
