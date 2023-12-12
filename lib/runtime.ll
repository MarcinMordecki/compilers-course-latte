; ModuleID = 'runtime.c'
source_filename = "runtime.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, %struct._IO_codecvt*, %struct._IO_wide_data*, %struct._IO_FILE*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type opaque
%struct._IO_codecvt = type opaque
%struct._IO_wide_data = type opaque

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.str.2 = private unnamed_addr constant [14 x i8] c"runtime error\00", align 1
@.str.3 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@stdin = external local_unnamed_addr global %struct._IO_FILE*, align 8

; Function Attrs: nofree nounwind uwtable
define dso_local void @printInt(i32 noundef %0) local_unnamed_addr #0 {
  %2 = tail call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i64 0, i64 0), i32 noundef %0)
  ret void
}

; Function Attrs: nofree nounwind
declare noundef i32 @printf(i8* nocapture noundef readonly, ...) local_unnamed_addr #1

; Function Attrs: nofree nounwind uwtable
define dso_local void @printString(i8* nocapture noundef readonly %0) local_unnamed_addr #0 {
  %2 = tail call i32 @puts(i8* nonnull dereferenceable(1) %0)
  ret void
}

; Function Attrs: noreturn nounwind uwtable
define dso_local void @error() local_unnamed_addr #2 {
  %1 = tail call i32 @puts(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([14 x i8], [14 x i8]* @.str.2, i64 0, i64 0))
  tail call void @exit(i32 noundef 1) #11
  unreachable
}

; Function Attrs: nofree nounwind
declare noundef i32 @puts(i8* nocapture noundef readonly) local_unnamed_addr #1

; Function Attrs: noreturn nounwind
declare void @exit(i32 noundef) local_unnamed_addr #3

; Function Attrs: nofree nounwind uwtable
define dso_local i32 @readInt() local_unnamed_addr #0 {
  %1 = alloca i32, align 4
  %2 = bitcast i32* %1 to i8*
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %2) #12
  %3 = call i32 (i8*, ...) @__isoc99_scanf(i8* noundef getelementptr inbounds ([3 x i8], [3 x i8]* @.str.3, i64 0, i64 0), i32* noundef nonnull %1)
  %4 = load i32, i32* %1, align 4, !tbaa !5
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %2) #12
  ret i32 %4
}

; Function Attrs: argmemonly mustprogress nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #4

; Function Attrs: nofree nounwind
declare noundef i32 @__isoc99_scanf(i8* nocapture noundef readonly, ...) local_unnamed_addr #1

; Function Attrs: argmemonly mustprogress nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #4

; Function Attrs: nounwind uwtable
define dso_local noalias i8* @readString() local_unnamed_addr #5 {
  %1 = alloca i8*, align 8
  %2 = alloca i64, align 8
  %3 = bitcast i8** %1 to i8*
  call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %3) #12
  store i8* null, i8** %1, align 8, !tbaa !9
  %4 = bitcast i64* %2 to i8*
  call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %4) #12
  store i64 0, i64* %2, align 8, !tbaa !11
  %5 = load %struct._IO_FILE*, %struct._IO_FILE** @stdin, align 8, !tbaa !9
  %6 = call i64 @getline(i8** noundef nonnull %1, i64* noundef nonnull %2, %struct._IO_FILE* noundef %5) #12
  %7 = icmp eq i64 %6, -1
  br i1 %7, label %8, label %10

8:                                                ; preds = %0
  %9 = call i32 @puts(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([14 x i8], [14 x i8]* @.str.2, i64 0, i64 0)) #12
  call void @exit(i32 noundef 1) #11
  unreachable

10:                                               ; preds = %0
  %11 = load i8*, i8** %1, align 8, !tbaa !9
  %12 = call i64 @strlen(i8* noundef nonnull dereferenceable(1) %11) #13
  %13 = shl i64 %12, 32
  %14 = ashr exact i64 %13, 32
  %15 = call noalias i8* @malloc(i64 noundef %14) #12
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %15, i8* align 1 %11, i64 %14, i1 false)
  %16 = add i64 %13, -4294967296
  %17 = ashr exact i64 %16, 32
  %18 = getelementptr inbounds i8, i8* %15, i64 %17
  store i8 0, i8* %18, align 1, !tbaa !13
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %4) #12
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %3) #12
  ret i8* %15
}

declare i64 @getline(i8** noundef, i64* noundef, %struct._IO_FILE* noundef) local_unnamed_addr #6

; Function Attrs: argmemonly mustprogress nofree nounwind readonly willreturn
declare i64 @strlen(i8* nocapture noundef) local_unnamed_addr #7

; Function Attrs: inaccessiblememonly mustprogress nofree nounwind willreturn
declare noalias noundef i8* @malloc(i64 noundef) local_unnamed_addr #8

; Function Attrs: mustprogress nofree nounwind uwtable willreturn
define dso_local noalias i8* @concat(i8* nocapture noundef readonly %0, i8* nocapture noundef readonly %1) local_unnamed_addr #9 {
  %3 = tail call i64 @strlen(i8* noundef nonnull dereferenceable(1) %0) #13
  %4 = tail call i64 @strlen(i8* noundef nonnull dereferenceable(1) %1) #13
  %5 = add i64 %4, %3
  %6 = shl i64 %5, 32
  %7 = add i64 %6, 4294967296
  %8 = ashr exact i64 %7, 32
  %9 = tail call noalias i8* @malloc(i64 noundef %8) #12
  %10 = shl i64 %3, 32
  %11 = ashr exact i64 %10, 32
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %9, i8* align 1 %0, i64 %11, i1 false)
  %12 = getelementptr inbounds i8, i8* %9, i64 %11
  %13 = shl i64 %4, 32
  %14 = ashr exact i64 %13, 32
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %12, i8* align 1 %1, i64 %14, i1 false)
  %15 = ashr exact i64 %6, 32
  %16 = getelementptr inbounds i8, i8* %9, i64 %15
  store i8 0, i8* %16, align 1, !tbaa !13
  ret i8* %9
}

; Function Attrs: argmemonly nofree nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #10

attributes #0 = { nofree nounwind uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nofree nounwind "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { noreturn nounwind uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { noreturn nounwind "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { argmemonly mustprogress nofree nosync nounwind willreturn }
attributes #5 = { nounwind uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #6 = { "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #7 = { argmemonly mustprogress nofree nounwind readonly willreturn "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #8 = { inaccessiblememonly mustprogress nofree nounwind willreturn "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #9 = { mustprogress nofree nounwind uwtable willreturn "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #10 = { argmemonly nofree nounwind willreturn }
attributes #11 = { noreturn nounwind }
attributes #12 = { nounwind }
attributes #13 = { nounwind readonly willreturn }

!llvm.module.flags = !{!0, !1, !2, !3}
!llvm.ident = !{!4}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{!"Debian clang version 14.0.6"}
!5 = !{!6, !6, i64 0}
!6 = !{!"int", !7, i64 0}
!7 = !{!"omnipotent char", !8, i64 0}
!8 = !{!"Simple C/C++ TBAA"}
!9 = !{!10, !10, i64 0}
!10 = !{!"any pointer", !7, i64 0}
!11 = !{!12, !12, i64 0}
!12 = !{!"long", !7, i64 0}
!13 = !{!7, !7, i64 0}
