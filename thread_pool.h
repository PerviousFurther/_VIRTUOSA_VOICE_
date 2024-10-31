#if !defined(THSW_H__)
#define THSW_H__

#include <utility>
#include <atomic>
#include <thread>
#include <semaphore>
#include <concepts>
#include <variant>
#include <tuple>
#include <exception>
#include <memory>
#include <latch>
#include <coroutine>

#if defined(_MSC_VER)
#	define no_unique_address [[msvc::no_unique_address]]
#else
#	define no_unique_address [[no_unique_address]]	
#endif

namespace grp
{
	template<typename Tag, typename...Args>
	concept tag_invocable = requires(Tag tag) { tag_invoke(tag, ::std::declval<Args>()...); };

	template<typename Tag, typename...Args> 
		requires(tag_invocable<Tag, Args...>)
	using tag_invoke_result_t = decltype(tag_invoke(::std::declval<Tag>(), ::std::declval<Args>()...));

	template<typename Tag, typename...Args>
	concept nothrow_tag_invocable = noexcept(tag_invoke(::std::declval<Tag>(), ::std::declval<Args>()...));

	namespace d
	{
		constexpr struct set_value_t 
		{
			template<typename R, typename...Ts>
			constexpr void operator()(R &&r, Ts&&...value) const noexcept
				requires(nothrow_tag_invocable<set_value_t, R&&, Ts&&...>)
			{ tag_invoke(*this, static_cast<R&&>(r), static_cast<Ts&&>(value)...); }
		} set_value;

		constexpr struct set_error_t 
		{
			template<typename R, typename E>
			constexpr void operator()(R &&r, E &&err) const noexcept
				requires(nothrow_tag_invocable<set_error_t, R &&, E &&>)
			{ tag_invoke(*this, static_cast<R &&>(r), static_cast<E &&>(err)); }
		} set_error;

		constexpr struct set_stopped_t
		{
			template<typename R>
			constexpr void operator()(R &&r) const noexcept
				requires(nothrow_tag_invocable<set_stopped_t, R&&>)
			{ tag_invoke(*this, static_cast<R &&>(r)); }
		} set_stopped;

		namespace d
		{
			template<typename T>
			struct valid_channel : ::std::false_type {};
			template<>
			struct valid_channel<set_value_t> : ::std::true_type {};
			template<>
			struct valid_channel<set_error_t> : ::std::true_type {};
			template<>
			struct valid_channel<set_stopped_t> : ::std::true_type {};

		}
		template<typename T>
		concept channel_tag = d::valid_channel<T>::value;
		template<channel_tag Tag, typename...Ts>
		struct sign_t { using tag_t = Tag; };

		namespace d
		{
			template<typename T>
			struct is_vaild_sign : ::std::false_type {};
			template<typename...Ts>
			struct is_vaild_sign<sign_t<set_value_t, Ts...>> : ::std::true_type {};
			template<typename E>
			struct is_vaild_sign<sign_t<set_error_t, E>> : ::std::true_type {};
			template<>
			struct is_vaild_sign<sign_t<set_stopped_t>> : ::std::true_type {};

			template<channel_tag T, typename...Ts>
			struct make_sign : ::std::type_identity<T(Ts...)> {};
			template<typename...Ts>
			struct make_sign<set_value_t, ::std::tuple<Ts...>> : ::std::type_identity<sign_t<set_value_t, Ts...>> {};
			template<typename F, typename...Ts>
			struct make_sign<set_error_t, F, Ts...> : ::std::type_identity<sign_t<set_value_t, F>> {};
			template<typename...Ts>
			struct make_sign<set_stopped_t, Ts...> : ::std::type_identity<sign_t<set_stopped_t>> {};
		}
		template<typename Sign>
		concept sign = d::is_vaild_sign<Sign>::value;
		template<channel_tag T, typename...Rs>
		using make_sign_t = typename d::make_sign<T, Rs...>::type;

		constexpr struct start_t
		{
			template<typename Op>
			constexpr void operator()(Op &op) const noexcept
				requires(nothrow_tag_invocable<start_t, Op&>)
			{ tag_invoke(*this, op); }

		} start{};

		constexpr struct connect_t
		{
			template<typename P, typename R>
			constexpr auto operator()(P &&s, R &&r) const
				noexcept(nothrow_tag_invocable<connect_t, P &&, R &&>)
				requires(tag_invocable<connect_t, P &&, R &&>)
			{ return tag_invoke(*this, static_cast<P &&>(s), static_cast<R &&>(r)); }

		} connect{};

		// obain attribute.

		constexpr struct query_allocator_t
		{
			template<typename Env>
			constexpr decltype(auto) operator()(Env const &e) const noexcept
				requires(nothrow_tag_invocable<query_allocator_t, Env &>)
			{ return tag_invoke(*this, e); }

		} query_allocator{};

		constexpr struct query_delegation_scheduler_t
		{
			template<typename Env>
			constexpr decltype(auto) operator()(Env &e) const noexcept
				requires(nothrow_tag_invocable<query_delegation_scheduler_t, Env &>)
			{ return tag_invoke(*this, e); }

		} query_delegation_scheduler{};

		enum forward_process_gurantee
		{
			weakly_parallel
			, parallel
			, concurrency
		};
		constexpr struct query_forward_process_gurantee_t
		{
			template<typename Env>
			constexpr decltype(auto) operator()(Env const &e) const noexcept
				requires(nothrow_tag_invocable<query_forward_process_gurantee_t, Env &>)
			{ return tag_invoke(*this, e); }

		} query_forward_process_gurantee{};

		constexpr struct query_completion_scheduler_t
		{
			template<typename Env>
			constexpr decltype(auto) operator()(Env const &e) const noexcept
				requires(nothrow_tag_invocable<query_completion_scheduler_t, Env &>)
			{ return tag_invoke(*this, e); }

		} query_completion_scheduler{};

		constexpr struct query_stop_token_t
		{
			template<typename Env>
			constexpr decltype(auto) operator()(Env const &e) const noexcept
				requires(nothrow_tag_invocable<query_stop_token_t, Env &>)
			{ return tag_invoke(*this, e); }

		} query_stop_token{};

		constexpr struct get_env_t
		{
			template<typename T>
			constexpr auto &operator()(T &envir_owner) const noexcept
				requires(nothrow_tag_invocable<get_env_t, T &>)
			{ return tag_invoke(*this, envir_owner); }

		} get_env{};

		constexpr struct schedule_t
		{
			template<typename T>
			constexpr auto operator()(T &scheduler) const 
				noexcept(nothrow_tag_invocable<schedule_t, T&>)
				requires(tag_invocable<schedule_t, T&>)
			{ return tag_invoke(*this, scheduler); }
		} schedule;

		constexpr struct await_t
		{
			template<typename T>
			constexpr auto operator()(T &&awaitable) const 
				requires(tag_invocable<await_t, T&&>)
			{ return tag_invoke(*this, static_cast<T&&>(awaitable)); }
		} await;

		struct procedure_part{};
		struct procedure_unity{};
	}

	template<typename P>
	concept procedure = ::std::move_constructible<::std::remove_cvref_t<P>> 
		&& requires{ typename::std::remove_cvref_t<P>::procedure_tag; }
	;
	template<typename E>
	concept executor = requires(E e) { e.get_scheduler(); } // need scheduler.
	;
	template<typename P>
	concept scheduler = /*::std::copy_constructible<::std::remove_reference_t<P>> &&*/ 
		::std::invocable<d::schedule_t, P&> // must accept no argument function or functor.
		;
	template<typename P>
	concept sync_token = ::std::move_constructible<::std::remove_cvref_t<P>> && requires(P s){ s.signal(); }
	;
	template<typename P>
	concept awaitable_object = ::std::copy_constructible<::std::remove_cvref_t<P>> && tag_invocable<d::await_t, P>
		;

	namespace d
	{
		template<typename...>
		constexpr auto always_false{false};

		template<typename From, typename To>
		concept ref_same_as = ::std::same_as<::std::remove_reference_t<From>, To>;
		template<typename From, typename To>
		concept quailified_same_as = ::std::same_as<::std::remove_cvref_t<From>, To>;
		template<typename Derived, typename Base>
		concept qualified_derived_from = ::std::derived_from<::std::remove_cvref_t<Derived>, Base>;

		namespace d
		{
			template<typename T, typename V>
			struct same_ref /*{ using type = V; }*/; // should be unreachable.
			template<typename T, typename V>
			struct same_ref<T &, V> { using type = V &; };
			template<typename T, typename V>
			struct same_ref<T const &, V> { using type = V const &; };
			template<typename T, typename V>
			struct same_ref<T &&, V> { using type = V &&; };
		}
		template<typename T, typename V>
		using same_ref_t = typename d::same_ref<T, ::std::remove_cvref_t<V>>::type;

		namespace d
		{
			template<typename T>
			struct is_tuple : ::std::false_type {};
			template<typename...Ts>
			struct is_tuple<::std::tuple<Ts...>> : ::std::true_type {};
		}
		template<typename T>
		static constexpr bool is_tuple_v = d::is_tuple<::std::remove_cvref_t<T>>::value;

		inline constexpr class apply_t
		{
			template<typename Fn, typename T, ::std::size_t...indices>
			static constexpr decltype(auto) impl(Fn &&fn, T &&tuple, ::std::index_sequence<indices...>)
			{
				if constexpr (::std::is_void_v<::std::invoke_result_t<Fn &&, ::std::tuple_element_t<indices, ::std::remove_cvref_t<T>>&&...>>)
				{
					static_cast<Fn &&>(fn)(::std::get<indices>(static_cast<T&&>(tuple))...);
					return::std::tuple<>{};
				} else
					return static_cast<Fn &&>(fn)(::std::get<indices>(static_cast<T&&>(tuple))...);
			}
		public:
			template<typename Fn, typename...Args>
			constexpr auto operator()(Fn &&fn, ::std::tuple<Args...> &&tup) const
				noexcept(::std::is_nothrow_invocable_v<Fn&&, Args&&...>)
				requires(::std::invocable<Fn&&, Args&&...>)
			{ return impl(static_cast<Fn&&>(fn), static_cast<::std::tuple<Args...> &&>(tup), ::std::make_index_sequence<sizeof...(Args)>{}); }

			template<typename Fn, typename...Args>
			constexpr auto operator()(Fn &&fn, ::std::tuple<Args...> &tup) const
				noexcept(::std::is_nothrow_invocable_v<Fn&&, Args&...>)
				requires(::std::invocable<Fn&&, Args&...>)
			{ return impl(static_cast<Fn &&>(fn), tup, ::std::make_index_sequence<sizeof...(Args)>{}); }

			template<typename Fn, typename T>
			constexpr auto operator()(Fn &&fn, T &&data) const
				noexcept(::std::is_nothrow_invocable_v<Fn &&, T&&>)
				requires(!is_tuple_v<T> && ::std::invocable<Fn&&, T&&>
			&& !tag_invocable<apply_t, Fn&&, T&&>)
			{ return static_cast<Fn &&>(fn)(static_cast<T &&>(data)); }

			template<typename Fn, typename T>
			constexpr decltype(auto) operator()(Fn &&fn, T &&data) const
				noexcept(nothrow_tag_invocable<apply_t, Fn&&, T&&>)
				requires(tag_invocable<apply_t, Fn&&, T&&>)
			{ return tag_invoke(*this, static_cast<Fn&&>(fn), static_cast<T&&>(data)); }
		} apply;

		namespace d
		{
			template<::std::size_t count>
			class drop_t
			{
				template<typename T, ::std::size_t...indices>
				static constexpr auto impl(T &&tuple, ::std::index_sequence<indices...>)
					->::std::tuple<::std::tuple_element_t<(count + indices), ::std::remove_cvref_t<T>>...>
				{ return{get<(count + indices)>(static_cast<T&&>(tuple))...}; }
			public:
				template<typename...Args>
				constexpr auto operator()(::std::tuple<Args...> tuple) const
					requires(count <= sizeof...(Args))
				{ return impl(::std::move(tuple), ::std::make_index_sequence<(sizeof...(Args) - count)>{}); }
			};
		}
		template<::std::size_t count = 1u>
		constexpr d::drop_t<count> drop{};

		inline constexpr class append_t
		{
			template<typename Tup, ::std::size_t...indices, typename...Apd>
			static auto impl(Tup &&t, ::std::index_sequence<indices...>, Apd&&...apd)
			{ return::std::make_tuple(get<indices>(static_cast<Tup&&>(t))..., static_cast<Apd&&>(apd)...); }

			template<typename Tup, ::std::size_t...indices, typename...Apd>
			static auto impl_r(Tup &&t, ::std::index_sequence<indices...>, Apd&&...apd)
			{ return::std::forward_as_tuple(get<indices>(static_cast<Tup&&>(t))..., static_cast<Apd&&>(apd)...); }

			template<typename T, typename F, typename...Apd>
			static auto impl_c(T &&t, F &&f, Apd&&...apd) 
				requires(tag_invocable<append_t, T&&, F&&>)
			{
				if constexpr (sizeof...(Apd) > 0u)
					return impl(tag_invoke(append_t{}, static_cast<T&&>(t), static_cast<F&&>(f)), static_cast<Apd&&>(apd)...);
				else
					return tag_invoke(append_t{}, static_cast<T&&>(t), static_cast<F&&>(f));
			}
		public:
			template<typename...Args, typename...Apd>
			constexpr auto operator()(::std::tuple<Args...> tuple, Apd&&...to_append) const
				-> ::std::tuple<Args..., ::std::decay_t<Apd>...>
				requires(((!::std::is_reference_v<Args>) && ...))
			{ return impl(std::move(tuple), ::std::make_index_sequence<sizeof...(Args)>{}, static_cast<Apd&&>(to_append)...); }

			template<typename...Args, typename...Apd>
			constexpr auto operator()(::std::tuple<Args...> tuple, Apd&&...to_append) const
				-> ::std::tuple<Args..., Apd&&...>
				requires(((::std::is_reference_v<Args>) || ...))
			{ return impl_r(std::move(tuple), ::std::make_index_sequence<sizeof...(Args)>{}, static_cast<Apd&&>(to_append)...); }

			template<typename T, typename F, typename...Apd>
			constexpr decltype(auto) operator()(T &&append_on, F &&first, Apd&&...rest) const
				requires(!is_tuple_v<::std::remove_cvref_t<T>>)
			{ return impl_c(static_cast<T&&>(append_on), static_cast<F&&>(first), static_cast<Apd&&>(rest)...); }
		} append{};

		inline constexpr class append_front_t
		{
			template<typename Tup, ::std::size_t...indices, typename...Apd>
			static auto impl(Tup &&t, ::std::index_sequence<indices...>, Apd&&...apd)
			{ return::std::make_tuple(static_cast<Apd&&>(apd)..., get<indices>(static_cast<Tup&&>(t))...); }

			template<typename Tup, ::std::size_t...indices, typename...Apd>
			static auto impl_r(Tup &&t, ::std::index_sequence<indices...>, Apd&&...apd)
			{ return::std::forward_as_tuple(static_cast<Apd&&>(apd)..., get<indices>(static_cast<Tup&&>(t))...); }

			template<typename T, typename F, typename...Apd>
			static auto impl_c(T &&t, F &&f, Apd&&...apd) 
				requires(tag_invocable<append_front_t, F&&, T&&>)
			{
				if constexpr (sizeof...(Apd) > 0u)
					return impl(tag_invoke(append_front_t{}, static_cast<F&&>(f), static_cast<T&&>(t)), static_cast<Apd&&>(apd)...);
				else
					return tag_invoke(append_front_t{}, static_cast<F&&>(f), static_cast<T&&>(t));
			}

		public:
			template<typename...Args, typename...Apd>
			constexpr auto operator()(::std::tuple<Args...> tuple, Apd&&...to_append) const
				-> ::std::tuple<::std::decay_t<Apd>..., Args...>
				requires(((!::std::is_reference_v<Args>) && ...))
			{ return impl(std::move(tuple), ::std::make_index_sequence<sizeof...(Args)>{}, static_cast<Apd&&>(to_append)...); }

			template<typename...Args, typename...Apd>
			constexpr auto operator()(::std::tuple<Args...> tuple, Apd&&...to_append) const
				-> ::std::tuple<Apd&&..., Args...>
				requires(((::std::is_reference_v<Args>) || ...))
			{ return impl_r(std::move(tuple), ::std::make_index_sequence<sizeof...(Args)>{}, static_cast<Apd&&>(to_append)...); }

			template<typename F, typename P>
			constexpr auto operator()(F &&first, P &&second) const
				requires(!is_tuple_v<F> && !tag_invocable<append_front_t, P&&, F&&> &&::std::constructible_from<::std::tuple<::std::decay_t<F>, ::std::decay_t<P>>, F&&, P&&>)
			{ return::std::make_tuple(static_cast<P&&>(second), static_cast<F&&>(first)); }

			template<typename T, typename F, typename...Apd>
			constexpr decltype(auto) operator()(T &&append_on, F &&first, Apd&&...rest) const
				requires(!is_tuple_v<T> && tag_invocable<append_front_t, F&&, T&&>)
			{ return impl_c(static_cast<T&&>(append_on), static_cast<F&&>(first), static_cast<Apd&&>(rest)...); }

		} append_front{};

		namespace d
		{
			template<typename T>
			struct construct_t
			{
				template<typename...Args>
				constexpr auto operator()(Args&&...args) const
					noexcept(::std::is_nothrow_constructible_v<T, Args&&...>) -> T
					requires(::std::constructible_from<T, Args&&...>)
				{ return{static_cast<Args&&>(args)...}; }
			};
		}
		template<typename T>
		constexpr d::construct_t<T> construct{};

		// @tparam front: later arguments should be invoked at function's front or back. 
		template<bool front, typename Fn, typename...Ts>
		struct packed_traits
		{
			using function = Fn;
			using parameters = ::std::tuple<Ts...>;

			template<typename...Args> static constexpr auto invocable{::std::invocable<Fn&, Ts&..., Args...>};
			template<typename...Args> static constexpr auto invocable_r{::std::invocable<Fn&&, Ts&&..., Args...>};
			template<typename...Args> static constexpr auto invocable_c{::std::invocable<Fn const&, Ts const&..., Args...>};
			template<typename...Args> static constexpr auto nothrow_invocable{::std::is_nothrow_invocable_v<Fn&, Ts&..., Args...>};
			template<typename...Args> static constexpr auto nothrow_invocable_r{::std::is_nothrow_invocable_v<Fn&&, Ts&&..., Args...>};
			template<typename...Args> static constexpr auto nothrow_invocable_c{::std::is_nothrow_invocable_v<Fn const&, Ts const&..., Args...>};

			template<typename Fnn, typename...Argss, typename Tup, ::std::size_t...indices>
			static constexpr decltype(auto) invoke(Fnn &&fn, Tup &&t, ::std::index_sequence<indices...>, Argss&&...args)
			{ return static_cast<Fn&&>(fn)(::std::get<indices>(static_cast<Tup&&>(t))..., static_cast<Argss&&>(args)...); }

		};
		template<typename Fn, typename...Ts>
		struct packed_traits<true, Fn, Ts...>
		{
			using function = Fn;
			using parameters = ::std::tuple<Ts...>;

			template<typename...Args> static constexpr auto invocable{::std::invocable<Fn&, Args..., Ts&...>};
			template<typename...Args> static constexpr auto invocable_r{::std::invocable<Fn&&, Args..., Ts&&...>};
			template<typename...Args> static constexpr auto invocable_c{::std::invocable<Fn const&, Args..., Ts const&...>};
			template<typename...Args> static constexpr auto nothrow_invocable{::std::is_nothrow_invocable_v<Fn&, Args..., Ts&...>};
			template<typename...Args> static constexpr auto nothrow_invocable_r{::std::is_nothrow_invocable_v<Fn&&, Args..., Ts&&...>};
			template<typename...Args> static constexpr auto nothrow_invocable_c{::std::is_nothrow_invocable_v<Fn const&, Args..., Ts const&...>};

			template<typename Fnn, typename...Argss, typename Tup, ::std::size_t...indices>
			static constexpr decltype(auto) invoke(Fnn &&fn, Tup &&t, ::std::index_sequence<indices...>, Argss&&...args)
			{ return static_cast<Fn&&>(fn)(static_cast<Argss&&>(args)..., ::std::get<indices>(static_cast<Tup&&>(t))...); }
		};
		template<typename Traits>
		class binded_fn
		{
			using function = typename Traits::function;
			using parameters = typename Traits::parameters;
			static constexpr auto indices_{::std::make_index_sequence<::std::tuple_size_v<parameters>>{}};
		public:
			template<typename Fnn, typename...Prmss>
			constexpr binded_fn(Fnn &&fn, Prmss &&...prms)
				noexcept(::std::is_nothrow_move_constructible_v<function> &&::std::is_nothrow_constructible_v<parameters, Prmss&&...>)
				requires(::std::constructible_from<function, Fnn&&> && ::std::constructible_from<parameters, Prmss&&...>)
			: f_{static_cast<Fnn &&>(fn)}
				, prms_{static_cast<Prmss &&>(prms)...}
			{}

			template<typename...Args>
			constexpr decltype(auto) operator()(Args&&...args) &
				noexcept(Traits::template nothrow_invocable<Args&&...>)
				requires(Traits::template invocable<Args&&...>)
			{ return Traits::invoke(f_, prms_, indices_, static_cast<Args>(args)...); }

			template<typename...Args>
			constexpr decltype(auto) operator()(Args&&...args) const &
				noexcept(Traits::template nothrow_invocable_c<Args&&...>)
				requires(Traits::template invocable_c<Args&&...>)
			{ return Traits::invoke(f_, prms_, indices_, static_cast<Args>(args)...); }

			template<typename...Args>
			constexpr decltype(auto) operator()(Args&&...args) &&
				noexcept(Traits::template nothrow_invocable_r<Args&&...>)
				requires(Traits::template invocable_r<Args&&...>)
			{ return Traits::invoke(static_cast<function&&>(f_), static_cast<parameters&&>(prms_), indices_, static_cast<Args>(args)...); }

			// should be unreachable.
			template<typename...Args> constexpr decltype(auto) operator()(Args&&...) const && = delete;
			template<typename...Args> constexpr decltype(auto) operator()(Args&&...) volatile & = delete;
			template<typename...Args> constexpr decltype(auto) operator()(Args&&...) volatile && = delete;
			template<typename...Args> constexpr decltype(auto) operator()(Args&&...) volatile const & = delete;
			template<typename...Args> constexpr decltype(auto) operator()(Args&&...) volatile const && = delete;
		private:
			no_unique_address function f_;
			no_unique_address parameters prms_;
		};

		template<typename Fn, typename...Args>
		constexpr auto bind_back(Fn fn, Args...args) noexcept
			-> binded_fn<packed_traits<true, Fn, Args...>>
			requires(::std::move_constructible<Fn> && ((::std::move_constructible<Args>) && ...))
		{ return {static_cast<Fn &&>(fn), static_cast<Args &&>(args)...}; }

		template<typename Fn, typename...Args>
		constexpr auto bind_front(Fn fn, Args...args) noexcept
			-> binded_fn<packed_traits<false, Fn, Args...>>
			requires(::std::move_constructible<Fn> && ((::std::move_constructible<Args>) && ...))
		{ return {static_cast<Fn &&>(fn), static_cast<Args &&>(args)...}; }

		static constexpr auto size_max{(::std::numeric_limits<::std::size_t>::max)()};

		// adaptor and closure.

		template<typename C>
		struct pipable_closure
		{
			template<typename>
			friend struct pipable_closure;

			template<typename T>
			friend constexpr auto operator|(T &&v, pipable_closure &&self) 
				noexcept(::std::is_nothrow_invocable_v<C&&, T&&>)
				//-> ::std::invoke_result_t<C&&, T&&>
				requires(::std::invocable<C&&, T&&>)
			{ return static_cast<C&&>(self)(static_cast<T&&>(v)); }
		};

		template<typename Fn>
		struct pipable_fn : pipable_closure<pipable_fn<Fn>>
		{
			constexpr pipable_fn(Fn &&fn)
				: f_{::std::move(fn)}
			{}

			template<typename T>
			auto operator()(T &&v) &&
				noexcept(::std::is_nothrow_invocable_v<Fn&&, T&&>)
				//-> ::std::invoke_result_t<Fn&&, T&&>
				requires(::std::invocable<Fn&&, T&&>)
			{ return::std::move(f_)(static_cast<T&&>(v)); }

		private:
			Fn f_;
		};

		template<typename From, typename To>
		concept pipable_with = requires { ::std::declval<From&&>() | ::std::declval<To&&>(); } && (!::std::integral<From>);

		template<typename T>
		concept capturable = ::std::invocable<::std::remove_cvref_t<T>&&>;

		template<typename C>
		struct capture_closure
		{
			template<typename T>
			constexpr auto operator&(T &&v) &&
				noexcept(::std::is_nothrow_invocable_v<C&&, T&&>)
				requires(::std::invocable<C&&, T&&> && capturable<::std::invoke_result_t<C&&, T&&>>)
			{ return static_cast<C&&>(*this)(static_cast<T &&>(v)); }

			template<typename T>
			friend constexpr auto operator|(T &&v, capture_closure &&self)
				noexcept(::std::is_nothrow_invocable_v<C&&> && ::std::is_nothrow_invocable_v<T&&, ::std::invoke_result_t<C&&>>)
				requires(::std::invocable<::std::invoke_result_t<C&&>, T&&>)
			{ return static_cast<C&&>(self)()(static_cast<T&&>(v)); }
		};

		// atomic utils.

		// single linked list and allow multiple producer and one consumer.
		template<typename Node>
		struct pins
		{
		public:
			constexpr pins() = default;

			void push(Node *n)
			{
				auto tail{t_.load(::std::memory_order_acquire)};
				while(!t_.compare_exchange_weak(tail, n, ::std::memory_order_release, ::std::memory_order_relaxed));
				if (!h_.load(::std::memory_order_relaxed))
					h_.store(n, ::std::memory_order_release);
				else
					tail->next(n);
			}
			template<typename...Args>
			auto &emplace(Args&&...args)
			{
				auto node = new Node{static_cast<Args&&>(args)...};
				push(node);
				return *node;
			}
			auto pop_all()
			{
				auto result{h_.exchange(nullptr, ::std::memory_order_acquire)};
				t_.store(nullptr, ::std::memory_order_release);
				return result;
			}

		private:
			::std::atomic<Node*> h_{nullptr};
			::std::atomic<Node*> t_{nullptr};
		};

		//namespace d
		//{
		//	template<typename T>
		//		requires(::std::destructible<T>)
		//	struct manual_lifetime
		//	{
		//		constexpr manual_lifetime() = default;
		//		manual_lifetime(manual_lifetime&&) = delete;
		//
		//		template<typename...Args>
		//		constexpr auto construct(Args&&...args) 
		//			noexcept(::std::is_nothrow_constructible_v<T, Args&&...>)
		//			requires(::std::constructible_from<T, Args&&...>)
		//		{ ::std::construct_at(reinterpret_cast<T*>(s_), static_cast<Args&&>(args)...); }
		//
		//		constexpr auto destruct() noexcept
		//		{ ::std::destroy_at(reinterpret_cast<T*>(s_)); }
		//
		//		constexpr T *operator&() noexcept { return::std::launder(reinterpret_cast<T*>(s_)); }
		//		constexpr T const *operator&() const noexcept { return::std::launder(reinterpret_cast<T const*>(s_)); }
		//		constexpr operator T&() & noexcept { return*::std::launder(reinterpret_cast<T*>(s_)); }
		//		constexpr operator T const &() const& noexcept { return*::std::launder(reinterpret_cast<T const*>(s_)); }
		//
		//		template<quailified_same_as<manual_lifetime> Self>
		//		friend constexpr auto &&get(Self &&self) noexcept 
		//		{ return static_cast<same_ref_t<Self&&, T>>(*::std::launder(reinterpret_cast<::std::conditional_t<::std::is_const_v<Self>, T const, T>*>(self.s_))); }
		//	private:
		//		char s_[sizeof(T)]{};
		//	};
		//}

		namespace d
		{
			template<typename F, typename...Rs>
			struct vc
			{
				static constexpr auto size{sizeof...(Rs) + 1u};

				template<::std::size_t index>
				auto operator()(::std::in_place_index_t<index>)
				{
					if constexpr (index == 0)
						return::std::type_identity<F>{};
					else if (sizeof...(Rs) > 0)
						return vc<Rs...>{}(::std::in_place_index<index - 1u>);
				}
				auto operator()() -> ::std::type_identity<::std::tuple<F, Rs...>>;

				template<procedure P> // add 
				consteval auto operator<<(::std::in_place_type_t<P>) const noexcept { return vc<F, Rs..., P>{}; }
				template<procedure P> // add 
				friend consteval auto operator>>(::std::in_place_type_t<P>, vc self) noexcept { return vc<P, F, Rs...>{}; }

				template<::std::size_t index> // split 
				consteval auto operator/(::std::in_place_index_t<index>) const 
				{
					if constexpr (index > sizeof...(Rs))
						return nullptr;
					else if constexpr (index == 0u) 
						return vc<F, Rs...>{}; 
					else 
						return vc<Rs...>{} / ::std::in_place_index<index - 1u>;
				}
			};
		}
		template<typename F, typename...Rs>
		constexpr d::vc<::std::decay_t<F>, ::std::decay_t<Rs>...> vd{};

		template<sync_token Tk, typename T>
		struct awaitable_future
		{
			using state_type = T;
			using result_type = tag_invoke_result_t<await_t, state_type&&>;

			constexpr awaitable_future(Tk tk, T st)
				: st_{::std::move(st)}
				, tk_{::std::move(tk)}
			{}
			awaitable_future(awaitable_future&&) noexcept = default;
			awaitable_future(awaitable_future const&) = default;

			inline result_type get() 
			{ return await(::std::move(st_)); }

			template<typename Fn>
			friend auto tag_invoke(apply_t, Fn &&fn, awaitable_future &&self) 
				requires(!::std::is_void_v<result_type>)
			{ return apply(static_cast<Fn&&>(fn), append_front(await(::std::move(self.st_)), ::std::move(self.tk_))); }

			template<typename Fn>
			friend auto tag_invoke(apply_t, Fn &&fn, awaitable_future &&self) 
				requires(::std::is_void_v<result_type>)
			{ await(::std::move(self.st_)); apply(static_cast<Fn&&>(fn), ::std::move(self.tk_)); }

		private:
			Tk tk_;
			state_type st_;
		};

		template<auto dfn = nullptr>
		struct vec;
		template<auto dfn>
		class launcher;
		class sync_source;
		template<::std::uint16_t index>
		struct sync_source_token; // express to proceduce.

		class sync_source // TODO: too much functionality...
		{
			using mask = ::std::uint16_t;
			using flags = ::std::atomic<mask>;

			static constexpr mask pending_{mask(-1)};

			static constexpr mask error_  {0x1u};
			static constexpr mask stopped_{0x2u};
			struct state
			{
				flags stage_{pending_};                     // current progress.
				::std::atomic_uchar rct_{0u};         // ref count token.
				::std::atomic_uchar rcs_{1u};         // ref count source.
				::std::atomic_uchar sta_{0u};         // status.
				flags min_stop_{pending_}; // should stop.
			};
			static bool stopped(mask flags)
			{ return (flags & error_) != 0u && (flags & stopped_) != 0u; }
			static bool waitable(mask m, mask stage, mask status)
			{ return !stopped(status) && (stage == pending_ || m < stage); }

			template<mask index>
			struct sync_point
			{
				friend sync_source;

				sync_point(sync_point const &other)
					: st_{other.st_}
				{ if(st_) st_->rct_.fetch_add(1u, ::std::memory_order_release); }

				sync_point(sync_point &&other) noexcept 
					: st_{::std::exchange(other.st_, nullptr)}
				{}

				sync_point& operator=(sync_point &&other) noexcept { ::std::swap(other.st_, st_); }

				sync_point& operator=(sync_point const &other)
				{ 
					if (st_ && 
						st_->rct_.fetch_sub(1u, ::std::memory_order_acq_rel) == 1u && 
						st_->rcs_.load(::std::memory_order_acquire) == 0u) 
						delete st_;
					if((st_ = other.st_) != nullptr) 
						st_->rct_.fetch_add(1u, ::std::memory_order_release); 
				}

				~sync_point() 
				{ 
					if (st_ && 
						st_->rct_.fetch_sub(1u, ::std::memory_order_acq_rel) == 1u && 
						st_->rcs_.load(::std::memory_order_acquire) == 0u) 
						delete st_; 
				}

				bool stop_possible() const noexcept 
				{ /*[[assume(st_->rcs_.load(::std::memory_order_acquire) != 0u)]]*/ 
					return waitable(index, st_->stage_.load(::std::memory_order_acquire), st_->sta_.load(::std::memory_order_acquire)); }

				bool request_stop() const noexcept 
				{
					if (stop_possible()) 
					{
						auto min{st_->min_stop_.load(::std::memory_order_acquire)};
						while(min > index && !st_->min_stop_.compare_exchange_weak(min, index));
						return min > index;
					} else 
						return false;
				}

				void wait() const noexcept 
				{  
					const auto index_{index};
					for(mask excepted;
						(excepted = st_->stage_.load(::std::memory_order_acquire)) != index;
						st_->stage_.wait(excepted, ::std::memory_order_acquire)); 
				}

				friend inline void tag_invoke(await_t, sync_point self) noexcept { return self.wait(); }

			private:
				constexpr sync_point(state *state) noexcept
					: st_{state} 
				{ if(state) state->rct_.fetch_add(1u, ::std::memory_order_release); }

			private:
				state *st_;
			};

		public:
			constexpr sync_source() 
				: st_{new state{}} {}

			sync_source(sync_source &&other) noexcept
				: st_{::std::exchange(other.st_, nullptr)}
			{}
			sync_source& operator=(sync_source &&) = delete;

			sync_source(sync_source const &other)
				: st_{other.st_}
			{ /*[[assume(st_ != nullptr)]]*/ st_->rcs_.fetch_add(1u, ::std::memory_order_release); }

			sync_source& operator=(sync_source const&) = delete;

			~sync_source()
			{
				if (st_ && 
					st_->rcs_.fetch_sub(1u, ::std::memory_order_acq_rel) == 1u &&  
					st_->rct_.load(::std::memory_order_acquire) == 0u)
					delete st_;
			}

			bool request_stop() noexcept 
			{ 
				auto cur{st_->sta_.load(::std::memory_order_acquire)};
				return !stopped(cur) && !(st_->stage_.exchange(stopped_ | cur, ::std::memory_order_release) & stopped_);
			}

			bool stop_requested() const noexcept
			{ return stopped(st_->sta_.load(::std::memory_order_acquire)); }

			bool error() const noexcept
			{ return (st_->sta_.load(::std::memory_order_acquire) & error_) != 0u; }

			template<mask index = 0u>
			constexpr sync_point<index> sync(::std::in_place_index_t<index> = ::std::in_place_index<index>) const noexcept { return{st_}; }

			template<mask index>
			constexpr sync_source_token<index> get_token() const noexcept { return {*this}; }

			void set(mask m) noexcept 
			{ 
				mask excepted(m);
				if (st_->stage_.compare_exchange_strong(excepted, m + 1u, ::std::memory_order_release, ::std::memory_order_relaxed)) [[likely]] 
				{
					st_->stage_.notify_all();
					if (st_->min_stop_.load(::std::memory_order_acquire) == excepted) 
						request_stop();
				}
			}

			bool set_start() noexcept
			{ mask bef{pending_}; return st_->stage_.compare_exchange_strong(bef, 0u, ::std::memory_order_release); }

			bool set_error() noexcept
			{ mask cur{st_->sta_.load(::std::memory_order_acquire)}; return st_->sta_.exchange(cur | error_, ::std::memory_order_release); }

		private:
			state *st_;
		};

		template<::std::uint16_t index>
		struct sync_source_token // express to proceduce.
		{
			friend sync_source;

			constexpr sync_source_token(sync_source_token &&other) noexcept
				: st_{::std::move(other.st_)}
			{ other.active_ = false; }

			void signal() noexcept 
			{ 
				if (active_) {
					active_ = false;
					st_.set(index); 
				} else
					::std::unreachable(); // do not signal if the token have been moved.
			}

			~sync_source_token() { if (active_) st_.set(index); }

		private:
			constexpr sync_source_token(sync_source st)
				: st_{st}
			{}

			bool active_{true};
			sync_source st_;
		};

		template<procedure P, sync_token Tk>
		struct invoke_traits
		{
			using noreturn_t = ::std::tuple<>;
			using sync_token_t = Tk;

			template<typename Prms>
			static constexpr bool invocable{::std::invocable<apply_t, P&&, ::std::invoke_result_t<append_front_t, Prms&&, sync_token_t&&>>};

			template<typename Prms>
			struct invoke_awaitable : ::std::false_type {};
			template<typename Prms>
				requires(::std::invocable<await_t, Prms>)
			struct invoke_awaitable<Prms>
			{ static constexpr auto value{::std::invocable<apply_t, P&&, ::std::invoke_result_t<append_front_t, ::std::invoke_result_t<await_t, Prms&&>, sync_token_t&&>>}; };

			template<typename Prms>
			static constexpr bool invocable_awaited{invoke_awaitable<Prms>::value};

			template<typename Prms>
			struct apply;
			template<typename Prms>
				requires(invocable<Prms>)
			struct apply<Prms> 
			{ 
				using unc_type = ::std::invoke_result_t<apply_t, P&&, ::std::invoke_result_t<append_front_t, Prms&&, sync_token_t&&>>; 
				using type = ::std::conditional_t<::std::is_void_v<unc_type>, noreturn_t, unc_type>;
			};
			template<typename Prms>
				requires(!invocable<Prms> && invocable_awaited<Prms>)
			struct apply<Prms>
			{ 
				using unc_type = ::std::invoke_result_t<apply_t, P&&, ::std::invoke_result_t<append_front_t, ::std::invoke_result_t<await_t, Prms&&>, sync_token_t&&>>;
				using type = ::std::conditional_t<::std::is_void_v<unc_type>, noreturn_t, unc_type>;
			};

			template<typename Prms>
				requires(invocable<Prms>)
			static auto invoke(P &&fn, Prms &&prms, sync_token auto token)
			{ 
				if constexpr (::std::is_void_v<typename apply<Prms>::type>) 
				{
					grp::d::apply(::std::move(fn), append_front(::std::move(prms), ::std::move(token)));
					return noreturn_t{};
				} else
					return grp::d::apply(::std::move(fn), append_front(::std::move(prms), ::std::move(token))); 
			}
			template<typename Prms>
				requires(!invocable<Prms> && invocable_awaited<Prms>)
			static auto invoke(P &&fn, Prms &&prms, sync_token auto token)
			{ 
				if constexpr (::std::is_void_v<typename apply<Prms>::type>) {
					grp::d::apply(::std::move(fn), awaitable_future{::std::move(token), ::std::move(prms)});
					return noreturn_t{};
				} else
					return grp::d::apply(::std::move(fn), awaitable_future{::std::move(token), ::std::move(prms)});
			}

			template<typename = void>
			struct error_flow : ::std::false_type {};
			template<typename T>
				requires(::std::invocable<P&&, ::std::exception_ptr>)
			struct error_flow<T> : ::std::true_type
			{
				using type = ::std::invoke_result_t<P&&, ::std::exception_ptr>;
				static constexpr auto nothrow  {::std::is_nothrow_invocable_v<P&&, ::std::exception_ptr>};
				static constexpr auto valid_ret{::std::same_as<type, bool> || ::std::same_as<type, void> /*|| ::std::invocable<apply_t, next_type, type&&>*/};
			};
		};

		template<auto dfn>
		class launcher
		{
		public:
			using descriptor_type = ::std::remove_cv_t<decltype(dfn)>;
			// a set that storage proceduces. have `friend get<::std::size_t{}>()`.
			using procset = typename::std::invoke_result_t<descriptor_type>::type;
			template<::std::size_t index> 
			using get_t = typename::std::invoke_result_t<descriptor_type, ::std::in_place_index_t<index>>::type;

			using noreturn_t = ::std::tuple<>;

			friend vec<dfn>;

			template<::std::size_t index>
			void express_error(std::exception_ptr ptr) noexcept
			{
				if constexpr (index < dfn.size) 
				{
					using flow = typename invoke_traits<get_t<index>, sync_source_token<index>>::template error_flow<>;
					if constexpr (flow::value) 
					{ // is error handler then propagate to it.
						using result_type = ::std::invoke_result_t<get_t<index>, ::std::exception_ptr>;
						if constexpr (::std::is_void_v<result_type>) 
						{ // will not product result.
							get<index>(static_cast<procset&&>(ps_))(ptr);
							express_error<index + 1u>(ptr);
						} else if constexpr (::std::same_as<bool, result_type>) {
							// return bool to decide error should be propagate to next or not.
							if(get<index>(static_cast<procset&&>(ps_))(ptr)) // return true, propagate to next.
								express_error<index + 1u>(ptr);
							else // return false.
								return;
						} else 
							express_value<index + 1u>(get<index>(static_cast<procset&&>(ps_))(ptr)); // TODO: should support resume to value channel?
					} else // skip.
						express_error<index + 1u>(ptr);
				} else 		
					r_.template emplace<1u>(ptr);
			}

			template<::std::size_t index = 0u, typename T = ::std::tuple<>>
			void express_value(T &&tuple = {}) noexcept
			{
				if constexpr (index < dfn.size) 
				{
					using traits = invoke_traits<get_t<index>, sync_source_token<index>>;
					try { // try execute.
						if (!st_.stop_requested()) [[likely]] // not stopped.
							express_value<index + 1u>(
								traits::invoke(
									get<index>(static_cast<procset&&>(ps_))
									, static_cast<T&&>(tuple)
									, st_.template get_token<index>()));
						else st_.set(index); // wake all waiting thread.
					} catch (...) { 
						// switch to error channel.
						express_error<index>(::std::current_exception());
						st_.set_error();
					}
				} else 
					r_.template emplace<2u>(static_cast<T&&>(tuple));
			}

			// traits result type.
			template<::std::size_t index = 0u, typename T = ::std::tuple<>>
			static auto get_r(::std::in_place_type_t<T> = ::std::in_place_type<T>) noexcept
			{
				if constexpr (index < dfn.size) 
				{ // 
					using traits = invoke_traits<get_t<index>, sync_source_token<index>>;
					using sync_token_t = typename traits::sync_token_t;
					static_assert(traits::template invocable<T&&> || traits::template invocable_awaited<T&&>,
								  "[FATAL]: proceduce cannot be invoked in this chain.");
					if constexpr (traits::template error_flow<>::value)
					{ // the procdure can handle error.
						using error_flow = typename traits::template error_flow<>;
						static_assert(error_flow::nothrow, "[FATAL]: error handler must be nothrow invocable.");
						static_assert(error_flow::valid_ref, "[FATAL]: error handler must return void or bool.");
					}
					if constexpr (traits::template invocable<T> || traits::template invocable_awaited<T>)
						return get_r<index + 1u>(::std::in_place_type<typename traits::template apply<T>::type>); 
				} else
					return::std::type_identity<T>{};
			}

			using unchecked_t = typename decltype(get_r())::type;
			template<typename T>
			struct check_result : ::std::type_identity<T> {};
			template<typename T>
				requires(::std::invocable<await_t, T&&>)
			struct check_result<T> : ::std::type_identity<::std::invoke_result_t<await_t, T>> {};

			constexpr launcher(procset ps)
				: ps_{::std::move(ps)}
			{}

		public:
			using result_type = typename check_result<unchecked_t>::type;

			launcher(launcher&&) = delete;

			// this will block current thread unless the.
			void operator()() &
				requires(::std::invocable<get_t<0>&&, sync_source_token<0>>) { 
				if (st_.set_start()) 
					express_value<0u>();
			}

			template<::std::size_t index = dfn.size>
			constexpr auto sync(::std::in_place_index_t<index> = ::std::in_place_index<index>) const& noexcept 
			{ return st_.template sync<index>(); }

			// do not invoke this unless the launcher is started.
			auto unwrap() noexcept(false) 
				requires(!::std::invocable<await_t, unchecked_t>)
			{
				await(this->template sync<dfn.size>());
				if (r_.index() == 2u) [[likely]] {
					if constexpr (!::std::same_as<noreturn_t, unchecked_t>)
						return get<2u>(::std::move(r_));
				} else if (r_.index() == 1u) 
					throw get<1u>(::std::move(r_));
				else
					throw::std::exception{}; // the operation is stopped.
			}
			// do not invoke this unless the launcher is started.
			auto unwrap() noexcept(false)
				requires(::std::invocable<await_t, unchecked_t>)
			{
				using await_result = ::std::invoke_result_t<await_t, unchecked_t>;
				await(this->template sync<dfn.size>());	
				if (r_.index() == 1u)
					throw get<1u>(::std::move(r_));
				else if (r_.index() == 2u) [[likely]]
					if constexpr (::std::is_void_v<await_result>) 
						await(get<2u>(::std::move(r_)));
					else
						return await(get<2u>(::std::move(r_)));
				else
					throw::std::exception{}; // the operation is stopped.
			}
		private:
			sync_source st_;
			no_unique_address procset ps_; // proceduces.
			::std::variant<::std::monostate, ::std::exception_ptr, unchecked_t> r_{}; // result.
		}; 

		// vec

		template<auto dfn>
		struct vec
		{
			using procedure_tag = procedure_unity;
			using descriptor_type = ::std::remove_cv_t<decltype(dfn)>;
			// a set that storage proceduces. have `friend get<::std::size_t{}>()`.
			using procset = typename::std::invoke_result_t<descriptor_type>::type;
			template<::std::size_t index> 
			using get_t = typename::std::invoke_result_t<descriptor_type, ::std::in_place_index_t<index>>::type;

			using launcher = launcher<dfn>;

			template<typename...Args>
			constexpr vec(Args&&...args)
				requires(::std::constructible_from<procset, Args&&...>)
			: prcs_{static_cast<Args&&>(args)...}
			{}

			constexpr launcher operator()() &
				noexcept(::std::is_nothrow_copy_constructible_v<procset>)
				requires(::std::copy_constructible<procset>)
			{ return{prcs_}; }

			constexpr launcher operator()() && 
				noexcept(::std::is_nothrow_move_constructible_v<procset>)
				requires(::std::move_constructible<procset>)
			{ return{::std::move(prcs_)}; }

			template<procedure Fn>
			friend constexpr auto tag_invoke(append_t, vec &&self, Fn fn)
			{ return apply(construct<vec<(dfn << ::std::in_place_type<Fn>)>>, ::std::tuple_cat(::std::move(self.prcs_), ::std::forward_as_tuple(static_cast<Fn&&>(fn)))); }
			template<procedure Fn>
			friend constexpr auto tag_invoke(append_front_t, Fn fn, vec &&self)
				-> vec<((::std::in_place_type<Fn>) >> dfn)>
			{ return apply(construct<vec<((::std::in_place_type<Fn>) >> dfn)>>, ::std::tuple_cat(::std::forward_as_tuple(static_cast<Fn&&>(fn)), ::std::move(self.prcs_))); }
		private:
			no_unique_address procset prcs_;
		};
		template<typename Dm>
		constexpr auto make_vec(Dm dm) { return vec<vd<Dm>>{::std::move(dm)}; }
		template<>
		class vec<nullptr>
		{
			template<auto desc>
			friend constexpr auto tag_invoke(append_t, vec<nullptr>, vec<desc> upper) { return upper; }
			template<procedure P>
			friend constexpr auto tag_invoke(append_t, vec<nullptr>, P p) { return make_vec(::std::move(p)); }
		public:
			using procedure_tag = procedure_unity;
		};
		using empty_vec = vec<nullptr>;
	}

	namespace d
	{
		struct just_t
		{
			template<typename T>
			struct proc
			{
				using procedure_tag = procedure_part;
				using value_type = T;

				template<typename...Args>
				constexpr proc(Args&&...args)
					requires(::std::constructible_from<value_type, Args&&...>)
				: v_{static_cast<Args&&>(args)...}
				{}

				constexpr auto operator()(sync_token auto) { return::std::move(v_); }

			private:
				value_type v_;
			};

			template<::std::movable...Args>
			constexpr auto operator()(Args...args) const
			{ return make_vec(proc<::std::tuple<Args...>>{::std::move(args)...}); }

			template<::std::movable...Args>
			constexpr auto operator()(::std::nullptr_t, Args...args) const
			{ return proc<::std::tuple<Args...>>{::std::move(args)...}; }
		};
	}
	constexpr d::just_t just{};

	namespace d
	{
		struct then_t
		{
			template<typename Fn>
			struct proc
			{
				using procedure_tag = procedure_part;

				constexpr proc(Fn &&f)
					: fn_{static_cast<Fn&&>(f)}
				{}

				template<typename...Args>
				constexpr auto operator()(sync_token auto, Args&&...args)
					requires(::std::invocable<Fn&&, Args&&...>)
				{ return::std::move(fn_)(static_cast<Args&&>(args)...); }

			private:
				Fn fn_;
			};

			template<procedure P, typename Fn> 
			constexpr auto operator()(P s, Fn fn) const
				requires(::std::same_as<typename P::procedure_tag, procedure_unity>)
			{ return append(::std::move(s), proc<Fn>{::std::move(fn)}); }

			template<typename Fn> 
			constexpr auto operator()(Fn fn) const
			{ return pipable_fn{bind_back(*this, ::std::move(fn))}; }
		};
	} 
	constexpr d::then_t then{};

	namespace d
	{
		struct sync_wait_t
		{
			template<procedure P>
			auto operator()(P &&s) const
				requires(::std::same_as<procedure_unity, typename::std::remove_reference_t<P>::procedure_tag>)
			{
				launcher launch{static_cast<P&&>(s)()};
				launch();
				return::std::move(launch).unwrap();
			}
		};
	}
	constexpr d::sync_wait_t sync_wait{};

	namespace d
	{
		struct on_t
		{
			template<typename Sche, procedure P>
			struct proc
			{
				using procedure_tag  = procedure_part;
				using schedule_type  = Sche;
				using procedure_type = P;

				constexpr proc(schedule_type sche, procedure_type s)
					: sc_{::std::move(sche)}
					, s_ {::std::move(s)}
				{}

				template<procedure PP, sync_token Tk>
				struct invoker
				{
					PP p_;
					Tk st_;

					auto operator()()
					{
						if constexpr (::std::is_void_v<decltype(sync_wait(::std::move(p_)))>) 
						{
							sync_wait(::std::move(p_)); 
							st_.signal();
						} else {
							auto result{sync_wait(::std::move(p_))};
							st_.signal();
							return result; 
						}
					}
				};

				template<typename...Args>
				auto operator()(sync_token auto st, Args&&...args) &&
					requires(::std::invocable<just_t, ::std::nullptr_t, Args&&...>)
				{ return::std::move(sc_)(invoker{append_front(::std::move(s_), just(nullptr, static_cast<Args&&>(args)...)), ::std::move(st)}); }

			private:
				schedule_type sc_;
				procedure_type s_;
			};

			template<typename Sche, procedure P>
			struct pipe : pipable_closure<pipe<Sche, P>>
			{
				constexpr pipe(Sche &&sche, P &&p)
					: sc_{::std::move(sche)}
					, p_{::std::move(p)}
				{}

				template<procedure O>
				constexpr auto operator()(O other) &&
				{ return append(::std::move(other), proc<Sche, P>{::std::move(sc_), ::std::move(p_)}); }

			protected:
				Sche sc_;
				P p_;
			};

			template<typename Sche, procedure P = empty_vec>
			struct capture : capture_closure<capture<Sche, P>>
			{
				constexpr capture(Sche &&sche, P &&p)
					: sc_{::std::move(sche)}
					, p_ {::std::move(p)}
				{}

				constexpr auto operator()() && { return pipe{::std::move(sc_), ::std::move(p_)}; }

				template<procedure NS>
				constexpr auto operator()(NS s) && 
				{ return capture{::std::move(sc_), append(::std::move(p_), ::std::move(s))}; }

				template<typename T>
				constexpr auto operator()(pipable_closure<T> &&p) && 
					requires(::std::invocable<T&&, P&&>)
				{ return capture{::std::move(sc_), ::std::move(p_) | ::std::move(p)}; }

			private:
				Sche sc_;
				P p_;
			};

			template<scheduler Sche, procedure U, procedure P>
			constexpr auto operator()(U upper, Sche &sche, P s) const noexcept
			{ return append(::std::move(upper), proc<Sche, P>{schedule(sche), ::std::move(s)}); }

			template<scheduler Sche, procedure P>
			constexpr auto operator()(Sche &sche, P send) const noexcept
			{ 
				using schedule_type = ::std::invoke_result_t<schedule_t, Sche&>;
				if constexpr (::std::same_as<typename P::procedure_tag, procedure_unity>)
					return pipe(schedule(sche), ::std::move(send)); 
				else if constexpr (::std::same_as<typename P::procedure_tag, procedure_part>)
					return pipe(schedule(sche), make_vec(::std::move(send)));
				else
					static_assert(always_false<P>, "[ERROR]: Unknown procedure type.");
			}

			template<scheduler Sche, typename B>
			constexpr auto operator()(Sche &sche, pipable_closure<B> &&binded) const noexcept
			{ return pipe(schedule(sche), empty_vec{} | ::std::move(binded)); }

			template<scheduler Sche>
			constexpr auto operator()(Sche &sche) const noexcept
			{ return capture{schedule(sche), empty_vec{}}; }
		};

	}
	constexpr d::on_t on{};

	namespace d
	{
		class single_thread_executor
		{
			using host_type = single_thread_executor;

			struct task
			{
				using invoke_type = void(*)(task*);

				virtual ~task() = default;
				virtual void launch() = 0;

				void next(task *nxt) noexcept { n_ = nxt; }
				task *next() noexcept { return n_; }

			private:
				task *n_{nullptr};
			};
			using task_list = pins<task>;

			template<bool del = true>
			static void execute_list(task_list *list)
			{
				auto node{list->pop_all()};
				while (node) 
				{
					node->launch(); // nothrow.
					if constexpr (del)
						delete::std::exchange(node, node->next());
				}
			}

			template<typename Fn>
			struct simple_task : task
			{
				using func_type = Fn;

				simple_task(func_type &&fn)
					: fn_{::std::move(fn)}
				{}

				void launch() override { fn_(); }

			private:
				func_type fn_;
			};

			template<typename Fn>
			class state : public::std::enable_shared_from_this<state<Fn>>
			{
				using weak_this = ::std::enable_shared_from_this<state<Fn>>;
				using func_type = Fn;
				using result_type = ::std::invoke_result_t<Fn&>;
				using storage_type 
					= ::std::conditional_t<::std::is_void_v<result_type>
					, ::std::variant<::std::monostate, ::std::exception_ptr>
					, ::std::variant<::std::monostate, ::std::exception_ptr, result_type>>;

				struct csche // completion scheduler or failure notify scheduler.
				{
					template<::std::invocable<> Fnn>
					void operator()(Fnn fn) { tl_->push(new simple_task{::std::move(fn)}); }

					task_list *tl_;
				};

				struct env
				{
					friend auto tag_invoke(query_stop_token_t, env const &self) noexcept { return self.st_; }
					friend auto tag_invoke(query_completion_scheduler_t, env const &self) noexcept { return csche{self.cpc_}; }
					friend auto tag_invoke(query_forward_process_gurantee_t, env const &) noexcept { return forward_process_gurantee::concurrency; }

					task_list *cpc_{};
					::std::stop_token st_{};
				};

				class observer
				{
					friend state;

					constexpr observer(::std::shared_ptr<state> st)
						: st_{st}
					{}

					template<ref_same_as<observer> Self>
					static result_type await(Self &&self)
					{
						auto state{self.st_.get()};
						state->st_.wait(false, ::std::memory_order_acquire);
						if constexpr (!::std::is_void_v<result_type>)
							if(state->r_.index() == 2u)
								return get<2u>(state->r_);
						if (state->r_.index() == 1u)
							::std::rethrow_exception(get<1u>(state->r_));
						else
							throw::std::exception{};
					}

					template<ref_same_as<observer> Self>
					friend result_type tag_invoke(await_t, Self &&self)
					{ return await(static_cast<Self&&>(self)); }

					::std::shared_ptr<state> st_;

				public:
					constexpr observer(observer const &) = default;
					constexpr observer(observer &&) = default;
				};

				class launcher : public task
				{
					friend state;

					launcher(::std::shared_ptr<state> st)
						: st_{st}
					{}

					void launch() noexcept override
					{
						auto state{st_.get()};
						if (!state->ss_.stop_requested()) 
						{
							try {
								if constexpr(::std::is_void_v<result_type>)
									state->fn_();
								else
									state->r_.template emplace<2u>(state->fn_());
								execute_list(&state->ccl_);
							} catch (...) {
								state->r_.template emplace<1u>(::std::current_exception());
								execute_list(&state->scl_);
							}
						}
						state->st_.store(true, ::std::memory_order_relaxed);
						state->st_.notify_all();
					}

					::std::shared_ptr<state> st_;
				};

			public:
				state(Fn fn)
					: fn_{::std::move(fn)}
				{}

				~state() { st_.wait(false, ::std::memory_order_acquire); }; // avoid memory leakage.

				friend env tag_invoke(get_env_t, state &self) noexcept { return {&self.ccl_, self.ss_.get_token()}; }

				observer observe() noexcept { return {weak_this::shared_from_this()}; }
				launcher *new_launcher() { return new launcher{weak_this::shared_from_this()}; }

			private:
				func_type fn_;
				task_list ccl_{}; // completion callback.
				task_list scl_{}; // failed callback.
				storage_type r_{};
				::std::stop_source ss_{};
				::std::atomic_bool st_{false};
			};

			class schr
			{
				friend host_type;

				class sche
				{
					friend schr;

					constexpr sche(task_list &ts)
						: ts_{&ts}
					{}

				public:
					template<typename Fn>
					auto operator()(Fn fn) requires(::std::invocable<Fn>)
					{
						auto stat{::std::make_shared<class state<Fn>>(::std::move(fn))};
						ts_->push(stat->new_launcher());
						return stat->observe();
					}

				private:
					task_list *ts_;
				};

			public:
				sche schedule() & { return{ts_}; }
				friend sche tag_invoke(schedule_t, schr &self) { return self.schedule(); }
			private:
				task_list ts_{};
			};
		public:
			single_thread_executor()
				: t_{[this]
			{
				st_.wait(0u, ::std::memory_order_acquire);
				execute_list(&scr_.ts_);
				st_.store(2u, ::std::memory_order_release);
				st_.notify_all();
			}}
			{}

			~single_thread_executor()
			{
				unsigned char started{0u};
				st_.compare_exchange_strong(started, 1u);
				if(started == 0u) st_.notify_one();
				t_.join();
			}

			constexpr auto &get_scheduler() noexcept { return scr_; }

			// start the tasks.
			void run() & noexcept 
			{ 
				st_.store(1u, ::std::memory_order_release); 
				st_.notify_one();
			}
			// wait all task were finished.
			void wait() & noexcept 
			{ st_.wait(1u, ::std::memory_order_acquire); }

		private:
			::std::thread t_;
			::std::atomic_uchar st_{0u};
			schr scr_{};
		};
	}
	using d::single_thread_executor;

}

#endif
