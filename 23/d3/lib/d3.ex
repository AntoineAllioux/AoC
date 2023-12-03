defmodule D3 do

  def sum(l) do
    Enum.reduce(l, 0, &(&1 + &2))
  end

  def compose(g, f) do
    fn x ->
      g.(f.(x))
    end
  end

  def is_digit(c) do
    c >= 48 and c <= 57
  end
  
  def is_symbol(c) do
    not is_digit(c) and c != 46
  end

  def height(schema) do
    Arrays.size(schema)
  end

  def width(schema) do
    Arrays.size(schema[0])
  end

  def neighbours(schema, i, j) do
    for x <- [i-1, i, i+1], x >= 0, x < height(schema),
        y <- [j-1, j, j+1], y >= 0, y < width(schema),
        x != i or y != j,
        do: {x, y} 
  end
  
  def check(schema, i, j) do
    Enum.reduce(neighbours(schema, i, j), false, fn {x, y}, acc -> 
      acc or is_symbol(schema[x][y]) 
    end)
  end

  def lower(schema, i, j) do
    if is_digit(schema[i][j-1]) do
      lower(schema, i, j-1)
    else
      j
    end
  end

  def upper(schema, i, j) do
    if is_digit(schema[i][j+1]) do
      upper(schema, i, j+1)
    else
      j
    end
  end
    
  def read_number(schema, i, j) do
    if is_digit(schema[i][j]) do
      j_low = lower(schema, i, j)
      j_up = upper(schema, i, j)

      Arrays.slice(schema[i], j_low..j_up)
      |> Arrays.to_list()
      |> List.to_integer()
    end
  end

  def get_schema() do
    File.read!("input")
    |> String.split("\n")
    |> Enum.map(&String.to_charlist/1)
    |> Enum.filter(fn row -> length(row) > 0 end)
    |> Enum.reduce(Arrays.empty(), fn row, acc ->
      Arrays.append(acc, Arrays.new(row))
    end)
  end
  
  def part1() do
    schema = get_schema()
    
    {_, _, numbers} = Enum.reduce(0..height(schema), {:searching, [], []}, fn i, {_, _, acc} ->
      Enum.reduce(0..width(schema), {:searching, [], acc}, fn j, {state, tmp, acc} ->
        case state do
          :reading_sure ->
            if is_digit(schema[i][j]) do
              {:reading_sure, tmp ++ [schema[i][j]], acc}
            else
              {:searching, [],  [tmp | acc]}
            end
          :reading_not_sure ->
            if is_digit(schema[i][j]) do
              if check(schema, i, j) do
                {:reading_sure, tmp ++ [schema[i][j]], acc}
              else
                {:reading_not_sure, tmp ++ [schema[i][j]], acc}
              end
            else
              {:searching, [], acc}
            end
          :searching ->
            if is_digit(schema[i][j]) do
              if check(schema, i, j) do
                {:reading_sure, [schema[i][j]], acc}
              else
                {:reading_not_sure, [schema[i][j]], acc}
              end
            else
              {:searching, [], acc}
            end
        end
      end)
    end)

    numbers
    |> Enum.map(&List.to_integer/1)
    |> sum
  end
  
  def part2() do

    schema = get_schema()

    numbers = Enum.reduce(0..height(schema), [], fn i, acc ->
      Enum.reduce(0..width(schema), acc, fn j, acc ->
        if schema[i][j] == ?* do
          neighbours = neighbours(schema, i, j)
          neighbour_numbers = Enum.reduce(neighbours, [], fn {i, j}, acc ->
            if is_digit(schema[i][j]) do
              [read_number(schema, i, j) | acc]
            else
              acc
            end
          end)
          [neighbour_numbers | acc]
        else
          acc
        end
      end)
    end)

    numbers
    |> Enum.map(compose(&Enum.dedup/1, &Enum.sort/1))
    |> Enum.filter(fn l -> length(l) == 2 end)
    |> Enum.map(fn [x, y] -> x * y end)
    |> sum
  end
end

IO.puts(D3.part1())
IO.puts(D3.part2())

