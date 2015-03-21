DIALYZER_OPTS = -Wunderspecs -Wrace_conditions -Wunmatched_returns -Werror_handling --statistics

rebuild: distclean all
