i = 0
for key, value in pairs(data) do
  i = i + value
end
if i < 5 then
  return 1
elseif i < 10 then
  return 2
else
  return 3
end
