record = "BPF"

cats = [
    "cats/bpf.cat",
    ]

cfgs = [
    "cfgs/new-web.cfg"
    ]

illustrative_tests = [
	"tests/CoRR+poonceonce+Once.litmus",
	"tests/CoRW+poonceonce+Once.litmus",
	"tests/CoWR+poonceonce+Once.litmus",
	"tests/CoWW+poonceonce.litmus",
	"tests/depencency_ordered_before.litmus",
	"tests/depencency_not_ordered_before.litmus",
	"tests/IRIW+fencembonceonces+OnceOnce.litmus",
	"tests/IRIW+poonceonces+OnceOnce.litmus",
	"tests/ISA2+poonceonces.litmus",
    "tests/ISA2+release+acquire+acquire.litmus",
    "tests/LockTwice.litmus",
	"tests/LB+fcas-addr-once+once-scas.litmus",
	"tests/LB+fcas-ctrlcvg-once+once-scas.litmus",
	"tests/LB+fcas-ctrl-once+once-scas.litmus",
	"tests/LB+fcas-data-once+once-scas.litmus",
	"tests/LB+poonceonces.litmus",
        "tests/LB+data+addr.litmus",
    "tests/LB+release-oa+acquire.litmus",
	"tests/MP+fcas-addr-fcas+scas-scas.litmus",
    "tests/MP+fcas-ctrl-fcas+scas-scas.litmus",
    "tests/MP+fcas-data-fcas+scas-scas.litmus",
    "tests/MP+poonceonces.litmus",
    "tests/MP+pooncerelease+poacquireonce.litmus",
	"tests/R+fencembonceonces.litmus",
	"tests/R+poonceonces.litmus",
    "tests/R+release+fence.litmus",
	"tests/S+atomiconce+data.litmus",
	"tests/SB+fence+fail_cmpxchg.litmus",
	"tests/SB+fencembonceonces.litmus",
	"tests/SB+fence+success_cmpxchg.litmus",
	"tests/SB+poonceonces.litmus",
	"tests/SB+rfionceonce-poonceonces.litmus",
	"tests/S+fence+addr.litmus",
	"tests/S+fence+ctrl-read.litmus",
	"tests/S+fence+ctrl-write.litmus",
	"tests/S+fence+data.litmus",
	"tests/S+onceatomic+data.litmus",
	"tests/S+poonceonces.litmus",
	"tests/WRC+poonceonces+Once.litmus",
	"tests/WRC+pooncerelease+fencermbonceonce+Once.litmus",
	"tests/W+RWC+poll+poaa+pola.litmus",
	"tests/X+addr-reads+corr-writes+data-rw.litmus",
	"tests/X-test-r2.litmus",
    "tests/C-LB-GRR+OB-OB-BPF.litmus",
    "tests/C-LB-GRR+OB-O+OB-OB-BPF.litmus",
    "tests/C-LB-GRR+OB-O+OB-O+OB-OB-BPF.litmus",
    "tests/C-LB-GRR+OB-O+OB-O+OB-O+OB-OB-BPF.litmus",
    "tests/C-LB-GRR+R-A+OB-OB-BPF.litmus",
    "tests/C-LB-GRR+R-A+OB-O+OB-OB-BPF.litmus",
    "tests/C-LB-GRR+R-A+OB-O+OB-O+OB-OB-BPF.litmus",
    "tests/C-LB-GRR+R-A+OB-O+R-A+OB-OB-BPF.litmus",
    "tests/C-LB-GRR+R-A+R-A+OB-OB-BPF.litmus",
    "tests/C-LB-GRR+R-A+R-A+OB-O+OB-OB-BPF.litmus",
    "tests/C-LB-GRR+R-A+R-A+R-A+OB-OB-BPF.litmus",
    "tests/C-LB-GRW+OB-OB-BPF.litmus",
	"tests/C-LB-GRW+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GRW+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GRW+OB-O+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GRW+R-A-BPF.litmus",
	"tests/C-LB-GRW+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-GRW+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GRW+R-A+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GRW+R-A+OB-O+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-GRW+R-A+R-A-BPF.litmus",
	"tests/C-LB-GRW+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-GRW+R-A+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GRW+R-A+R-A+R-A-BPF.litmus",
	"tests/C-LB-GRW+R-A+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-GRW+R-A+R-A+R-A+R-A-BPF.litmus",
	"tests/C-LB-GWR+OB-OB-BPF.litmus",
	"tests/C-LB-GWR+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GWR+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GWR+OB-O+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GWR+R-A-BPF.litmus",
	"tests/C-LB-GWR+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-GWR+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GWR+R-A+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GWR+R-A+OB-O+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-GWR+R-A+R-A-BPF.litmus",
	"tests/C-LB-GWR+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-GWR+R-A+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GWR+R-A+R-A+R-A-BPF.litmus",
	"tests/C-LB-GWR+R-A+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-GWR+R-A+R-A+R-A+R-A-BPF.litmus",
	"tests/C-LB-GWW+OB-OB-BPF.litmus",
	"tests/C-LB-GWW+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GWW+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GWW+OB-O+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GWW+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-GWW+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GWW+R-A+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GWW+R-A+OB-O+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-GWW+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-GWW+R-A+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-GWW+R-A+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LRR+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRR+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRR+OB-O+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRR+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LRR+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRR+R-A+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRR+R-A+OB-O+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LRR+R-A+R-A-BPF.litmus",
	"tests/C-LB-LRR+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LRR+R-A+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRR+R-A+R-A+R-A-BPF.litmus",
	"tests/C-LB-LRR+R-A+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LRR+R-A+R-A+R-A+R-A-BPF.litmus",
	"tests/C-LB-LRW+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+OB-O+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+OB-Ov-BPF.litmus",
	"tests/C-LB-LRW+O-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+O-O+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+R-A+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+R-A+OB-O+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+R-A+O-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+R-A+R-A-BPF.litmus",
	"tests/C-LB-LRW+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+R-A+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+R-A+R-A+R-A-BPF.litmus",
	"tests/C-LB-LRW+R-A+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LRW+R-A+R-A+R-A+R-A-BPF.litmus",
	"tests/C-LB-LRW+R-A+R-A+R-A+RQ-A-BPF.litmus",
	"tests/C-LB-LRW+R-A+R-A+RQ-A-BPF.litmus",
	"tests/C-LB-LRW+R-A+R-A+RQ-A+R-A-BPF.litmus",
	"tests/C-LB-LRW+R-A+RQ-A-BPF.litmus",
	"tests/C-LB-LRW+R-A+RQ-A+R-A-BPF.litmus",
	"tests/C-LB-LRW+RQ-A+R-A-BPF.litmus",
	"tests/C-LB-LWR+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LWR+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LWR+OB-O+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LWR+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LWR+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LWR+R-A+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LWR+R-A+OB-O+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LWR+R-A+R-A-BPF.litmus",
	"tests/C-LB-LWR+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LWR+R-A+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LWR+R-A+R-A+R-A-BPF.litmus",
	"tests/C-LB-LWR+R-A+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LWR+R-A+R-A+R-A+R-A-BPF.litmus",
	"tests/C-LB-LWW+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LWW+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LWW+OB-O+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LWW+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LWW+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LWW+R-A+OB-O+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LWW+R-A+OB-O+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LWW+R-A+R-A-BPF.litmus",
	"tests/C-LB-LWW+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LWW+R-A+R-A+OB-O+OB-OB-BPF.litmus",
	"tests/C-LB-LWW+R-A+R-A+R-A-BPF.litmus",
	"tests/C-LB-LWW+R-A+R-A+R-A+OB-OB-BPF.litmus",
	"tests/C-LB-LWW+R-A+R-A+R-A+R-A-BPF.litmus",
	"tests/C-RW-B+RW-B-BPF.litmus",
	"tests/C-RW-B+RW-B+RW-B-BPF.litmus",
	"tests/C-RW-B+RW-B+RW-B+RW-B-BPF.litmus",
	"tests/C-RW-B+RW-B+RW-B+RW-B+RW-B-BPF.litmus",
	"tests/C-RW-B+RW-B+RW-B+RW-B+RW-B+RW-B-BPF.litmus",
	"tests/C-RW-B+RW-B+RW-B+RW-B+RW-B+RW-B+RW-B-BPF.litmus",
	"tests/C-RW-B+RW-B+RW-B+RW-B+RW-B+RW-B+RW-B+RW-B-BPF.litmus",
	"tests/C-RW-r+RW-a-BPF.litmus",
	"tests/C-RW-r+RW-a+RW-B-BPF.litmus",
	"tests/C-RW-r+RW-a+RW-B+RW-B-BPF.litmus",
	"tests/C-RW-r+RW-a+RW-B+RW-B+RW-B-BPF.litmus",
	"tests/C-RW-r+RW-a+RW-B+RW-B+RW-B+RW-B-BPF.litmus",
	"tests/C-RW-r+RW-a+RW-B+RW-B+RW-B+RW-B+RW-B-BPF.litmus",
	"tests/C-RW-r+RW-a+RW-B+RW-B+RW-B+RW-B+RW-B+RW-B-BPF.litmus",
    "tests/2+2W+release+fence.litmus",
    "tests/Z6.3+fence+fence+acquire.litmus",
    "tests/amo-sequence-1.litmus",
    "tests/amo-sequence-2.litmus",
]
