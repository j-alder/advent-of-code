defmodule Y2024.D01 do
  def part_one(input) do
    input
    |> String.split("\n")
    |> Enum.map(&String.split(&1, "   "))
    |> Enum.map(fn [left, right] -> {String.to_integer(left), String.to_integer(right)} end)
    |> Enum.sort_by(fn {left, _} -> left end)
    |> Enum.sort_by(fn {_, right} -> right end)
    |> Enum.map(fn {left, right} -> abs(left - right) end)
    |> Enum.sum()
    |> IO.puts()
  end

  def input do
    File.read!("../input/y2024/d01.txt")
  end
end

input = Y2024.D01.input()
Y2024.D01.part_one(input)
