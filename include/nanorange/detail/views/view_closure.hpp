#ifndef NANORANGE_DETAIL_VIEWS_VIEW_CLOSURE_HPP
#define NANORANGE_DETAIL_VIEWS_VIEW_CLOSURE_HPP

#include <compare>

#include <nanorange/detail/views/range_adaptors.hpp>
#include <nanorange/views/all.hpp>
#include <nanorange/views/interface.hpp>

#include <tuple>
#include <type_traits>

NANO_BEGIN_NAMESPACE
	template<semiregular F, copy_constructible... Args>
	class view_closure;

	template<typename>
	inline constexpr auto __is_view_closure_v = false;

	template<typename F, typename... Args>
	inline constexpr auto __is_view_closure_v<view_closure<F, Args...>> = true;

	template<typename T>
	concept __is_view_closure = __is_view_closure_v<std::remove_cvref_t<T>>;

	struct __dummy_view : view_interface<__dummy_view> {
		static constexpr void* begin() noexcept { return nullptr; }
		static constexpr void* end() noexcept { return nullptr; }
	};

	template<std::size_t N, typename T, typename... Args>
	requires (sizeof...(Args) >= N)
	struct __pack_type : __pack_type<N - 1, Args...> {};

	template<typename T, typename... Args>
	struct __pack_type<0, T, Args...> {
		using type = T;
	};

	template<std::size_t N, typename... Args>
	using __pack_type_t = typename __pack_type<N, Args...>::type;

	template<typename F, typename R, typename... Args>
	concept __closure =
		same_as<F, __dummy_view> and
		invocable<__pack_type_t<0, Args...>&, R> and
		invocable<__pack_type_t<1, Args...>&, std::invoke_result_t<__pack_type_t<0, Args...>&, R>>;

	template<typename F, typename R, typename... Args>
	concept __range_pipeline =  invocable<F, R, Args...> and view<std::invoke_result_t<F, R, Args...>>;

	template<typename F, typename R, typename... Args>
	concept __closure_pipeline =
		__closure<F, R, Args...> and
		view<std::invoke_result_t<__pack_type_t<1, Args...>&, std::invoke_result_t<__pack_type_t<0, Args...>&, R>>>;

	template<typename F, typename R, typename... Args>
	concept __pipeline = __range_pipeline<F, R, Args...> or __closure_pipeline<F, R, Args...>;

	template<typename F, typename R, typename... Args>
	concept __pipeable = viewable_range<R> and __pipeline<F, R, Args...>;

	template<semiregular F, copy_constructible... Args>
	class view_closure {
	public:
		view_closure() = default;

		template<typename... Us>
		constexpr explicit view_closure(F, Us&&... args)
		: args_(std::make_tuple(std::forward<Us>(args)...))
		{}

		template<typename... Us>
		constexpr explicit view_closure(F, Us&&... args)
		requires same_as<F, __dummy_view>
		: args_(std::make_tuple(std::forward<Us>(args)...))
		{}

		template<input_range R>
		requires __pipeable<F, R, Args...>
		[[nodiscard]] constexpr auto operator()(R&& r) &
		{
			return apply(*this, std::forward<R>(r), std::index_sequence_for<Args...>());
		}

		template<input_range R>
		requires __pipeable<F, R, Args...>
		[[nodiscard]] constexpr auto operator()(R&& r) const&
		{
			return apply(*this, std::forward<R>(r), std::index_sequence_for<Args...>());
		}

		template<input_range R>
		requires __pipeable<F, R, Args...>
		[[nodiscard]] constexpr auto operator()(R&& r) &&
		{
			return apply(std::move(*this), std::forward<R>(r), std::index_sequence_for<Args...>());
		}

		template<input_range R>
		requires __pipeable<F, R, Args...>
		[[nodiscard]] constexpr auto operator()(R&& r) const&&
		{
			return apply(std::move(*this), std::forward<R>(r), std::index_sequence_for<Args...>());
		}

		template<input_range R, typename C>
		requires same_as<std::remove_cvref_t<C>, view_closure> and __pipeable<F, R, Args...>
		[[nodiscard]] constexpr friend auto operator|(R&& r, C&& closure)
		{
			return std::forward<C>(closure)(std::forward<R>(r));
		}

		template<__is_view_closure C1, __is_view_closure C2>
		requires same_as<std::remove_cvref_t<C1>, view_closure>
		[[nodiscard]] constexpr friend auto operator|(C1&& c1, C2&& c2)
		{
			return view_closure<__dummy_view, view_closure, std::remove_cvref_t<C2>>(__dummy_view(), std::forward<C1>(c1), std::forward<C2>(c2));
		}
	private:
		[[no_unique_address]] std::tuple<std::remove_cvref_t<Args>...> args_; // @\expos@

		// @\expos@
		template<class Self, class R, std::size_t... Is>
		[[nodiscard]] static constexpr auto apply(Self&& self, R&& r, std::index_sequence<Is...>)
		{
			if constexpr (same_as<F, __dummy_view>) {
				if constexpr (std::is_lvalue_reference_v<Self>) {
					return get<1>(self.args_)(get<0>(self.args_)(std::forward<R>(r)));
				}
				else {
					return std::move(get<1>(self.args_))(std::move(get<0>(self.args_))(std::forward<R>(r)));
				}
			}
			else if constexpr (std::is_lvalue_reference_v<Self>) {
				return F()(std::forward<R>(r), get<Is>(self.args_)...);
			}
			else {
				return F()(std::forward<R>(r), std::move(get<Is>(self.args_))...);
			}
		}
	};

	template<semiregular F, copy_constructible... Args>
	view_closure(F, Args&&...) -> view_closure<F, Args...>;
NANO_END_NAMESPACE

#endif // NANORANGE_DETAIL_VIEWS_VIEW_CLOSURE_HPP
