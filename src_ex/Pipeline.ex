defmodule Pipeline do

    @type result :: {:ok, term} | {:error, term}
    @type pipeline_fun :: (term -> result)

    @spec bind(term, [pipeline_fun]) :: result
    def bind arg, funs do
        Enum.reduce funs, {:ok, arg},
                    fn
                        f, {:ok, prev_res} -> f.(prev_res)
                        _, {:error, reason} -> {:error, reason}
                    end
    end


    @spec sequence([result]) :: result
    def sequence [] do {:ok, []} end
    def sequence [{:error, reason} | _] do {:error, reason} end
    def sequence [{:ok, val} | tail] do
        case sequence tail do
            {:ok, list} -> {:ok, [val | list]}
            {:error, error} -> {:error, error}
        end
    end


    @spec sample(integer) :: result
    def sample arg do
        bind arg, [
            fn a -> {:ok, a + 10} end,
            fn a ->
                if a > 0 do {:ok, a * 2}
                else {:error, {:invalid_arg, a}}
                end
            end,
            fn a -> {:ok, [a, a, a]} end,
            fn a_list -> Enum.map a_list, fn a -> {a, a + 1} end end
        ]
    end


    def sample2 do
        list1 = [{:ok, 1}, {:ok, 2}, {:ok, 3}, {:ok, 4}]
        res1 = sequence list1
        IO.puts "list1: #{inspect list1}, res1: #{inspect res1}"

        list2 = [{:ok, 1},  {:error, :something_wrong_2}, {:error, :something_wrong_3}, {:ok, 4}]
        res2 = sequence list2
        IO.puts "list2: #{inspect list2}, res2: #{inspect res2}"
    end

end