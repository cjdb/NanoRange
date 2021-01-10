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
		constexpr explicit view_closure(F closure, Us&&... args)
		: closure_(closure)
		, args_(std::make_tuple(std::forward<Us>(args)...))
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
		[[no_unique_address]] F closure_;
		[[no_unique_address]] std::tuple<std::remove_cvref_t<Args>...> args_;

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
				return self.closure_(std::forward<R>(r), get<Is>(self.args_)...);
			}
			else {
				return std::move(self.closure_)(std::forward<R>(r), std::move(get<Is>(self.args_))...);
			}
		}
	};

	template<semiregular F, copy_constructible... Args>
	view_closure(F, Args&&...) -> view_closure<F, Args...>;

	template<class F>
	class range_adaptor;

	template<typename F1, typename F2>
	auto compose_pipeline(range_adaptor<F1>, range_adaptor<F2>);

	template<class F>
	class range_adaptor : public F {
	public:
		constexpr range_adaptor(F&& f)
		: F(std::move(f))
		{}

		template<input_range R, typename... Args>
		requires invocable<F&, R, Args...>
		[[nodiscard]] constexpr auto operator()(R&& r, Args&&... args) const
		{
			return std::invoke(static_cast<F const&>(*this), std::forward<R>(r), std::forward<Args>(args)...);
		}

		template<typename... Args>
		[[nodiscard]] constexpr auto operator()(Args&&... args) const
		{
			return view_closure(static_cast<F const&>(*this), std::forward<Args>(args)...);
		}

		template<input_range R>
		requires invocable<F&, R>
		[[nodiscard]] constexpr friend auto operator|(R&& r, range_adaptor const& adaptor)
		{
			return adaptor(r);
		}

		template<typename F2>
		[[nodiscard]] constexpr auto operator|(range_adaptor<F2> const& other) const
		{
			return compose_pipeline(*this, other);
		}
	};

	template<typename F>
	range_adaptor(F closure) -> range_adaptor<F>;

	template<typename F1, typename F2>
	auto compose_pipeline(range_adaptor<F1> x, range_adaptor<F2> y)
	{
		return range_adaptor([x = std::move(x), y = std::move(y)]<viewable_range R>(R&& r) {
			return std::invoke(y, std::invoke(x, std::forward<R>(r)));
		});
	}
NANO_END_NAMESPACE

#endif // NANORANGE_DETAIL_VIEWS_VIEW_CLOSURE_HPP
