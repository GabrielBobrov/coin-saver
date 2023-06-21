import { ComponentFixture, TestBed } from '@angular/core/testing';

import { QuemSomosPageComponent } from './quem-somos-page.component';

describe('QuemSomosPageComponent', () => {
  let component: QuemSomosPageComponent;
  let fixture: ComponentFixture<QuemSomosPageComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ QuemSomosPageComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(QuemSomosPageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
