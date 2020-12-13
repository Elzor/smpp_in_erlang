{logdir, "./logs/"}.

{ct_hooks, [smpp_example_ct_hook, cth_surefire]}.

{specs, join, [
    "../common/suites.spec",
    "../esme/suites.spec"
]}.